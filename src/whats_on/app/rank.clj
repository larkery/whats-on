(ns whats-on.app.rank
  "Turning items + taste into ranked suggestions.

  Taste is a set of examples, each an embedding with a weight:
    ⭐ must go +1.5 · ✓ booked +1.5 · past history +1.5 · 👍 liked +1 · 'I like…' +1
    😐 meh 0 (kept for later; doesn't score) · 👎 disliked −1 · 'not interested in…' −1
  An event scores by how close it is to its nearest positive examples. Using the
  nearest few examples (not an average of everything) lets very different
  tastes coexist — liking both folk gigs and history talks doesn't blur into
  'folky history'.

  Vectors are first centred on the mean of the upcoming events: bge's vectors
  share a big common component, so raw cosines bunch up around 0.6 and a few
  events end up close to everything. The bench (bench.clj) showed centring,
  without the old dislike penalty, ranks held-out likes much better."
  (:require [clojure.string :as str]
            [whats-on.app.embed :as embed]
            [whats-on.app.store :as store]))

;;; --- coarse kinds, for filtering ------------------------------------------

(def kinds
  ;; first match wins; matched against source, category, tags and title
  [[:film       "Film"            #"(?i)^(watershed|cube|megascreen)\b|\bfilm\b|cinema|screening"]
   [:comedy     "Comedy"          #"(?i)comedy|stand.?up|\bimprov"]
   [:talks      "Talks & books"   #"(?i)\btalks?\b|lecture|\bbooks?\b|author|\bideas\b|poetry|in conversation|literary|spoken word|book club"]
   [:classical  "Classical"       #"(?i)classical|orchestr|\bchoir|choral|\borgan\b|recital|opera|chamber|string quartet|baroque"]
   [:club       "Club nights"     #"(?i)club night|\bdj\b|techno|\bhouse\b|drum ?(and|&|n) ?bass|\bdnb\b|garage|jungle|rave"]
   [:music      "Music"           #"(?i)music|\bgig\b|jazz|folk|rock|\bpop\b|indie|hip.?hop|soul|blues|metal|punk|electronic|ambient|americana|singer|songwriter|\bband\b"]
   [:theatre    "Theatre & dance" #"(?i)theatre|\bplays?\b|musical|panto|dance|ballet|cabaret|circus|\bdrag\b|puppet"]
   [:family     "Family"          #"(?i)family|\bkids\b|children|toddler|under (5|fives)"]
   [:exhibition "Exhibitions"     #"(?i)exhibition|gallery|museum|\btours?\b|\bwalks?\b"]])

(def kind-labels (into (array-map) (map (fn [[k label]] [k label])) (conj kinds [:other "Other"])))

(defn kind [{:keys [source category tags title]}]
  (let [text (str/join " | " (concat [source category] tags))]
    (or (some (fn [[k _ re]] (when (re-find re text) k)) kinds)
        (some (fn [[k _ re]] (when (re-find re (str title)) k)) kinds)
        :other)))

;;; --- taste profile --------------------------------------------------------

(def verdict-weights {"love" 1.5 "booked" 1.5 "like" 1.0 "meh" 0.0 "dislike" -1.0})

(defn examples
  "All taste examples as {:vec :weight :label}. Computes any missing embeddings."
  [ds]
  (concat
   (for [{:keys [key verdict text title venue]} (store/feedback ds)
         :let [w (verdict-weights verdict)]
         :when (and w (not (str/blank? text)))]
     {:vec (store/vector-for ds text) :weight w
      :key key :verdict verdict :title title :venue venue
      :label (str title (when venue (str " at " venue)))
      :why (cond (neg? w) "you disliked" (zero? w) "you were meh about" :else "you liked")})
   (for [{:keys [text weight]} (store/statements ds)]
     ;; statements read like queries ("I love folk music"), so use bge's query prefix
     {:vec (store/vector-for ds (str embed/query-prefix text)) :weight weight
      :label (str "“" text "”") :why "you said"})
   (for [{:keys [title venue notes]} (store/history ds)]
     {:vec (store/vector-for ds (str/join ". " (remove str/blank? [title (some->> venue (str "at ")) notes])))
      :weight 1.5 :title title :venue venue
      :label (str title (when venue (str " at " venue))) :why "you went to"})))

(def ^:private top-k 3)

;; similarity above which a dislike counts against an event, and a like is
;; worth citing as the reason; tuned to raw bge cosines
(def strong-sim 0.62)

(defn taste-score
  "→ {:score s :because example :unlike example} or nil if the item has no
  embedding yet. `penalty-sim`: a dislike closer than this counts against;
  `because-sim`: a like closer than this is cited as the reason."
  ([examples v] (taste-score examples v {}))
  ([examples ^floats v {:keys [penalty-sim because-sim] :or {penalty-sim strong-sim because-sim strong-sim}}]
   (when v
     (let [sims (map #(assoc % :sim (embed/cosine (:vec %) v)) examples)
           pos (->> sims (filter #(pos? (:weight %))) (sort-by :sim >))
           neg (->> sims (filter #(neg? (:weight %))) (sort-by :sim >))
           pos-score (if (seq pos)
                       (let [top (take top-k pos)]
                         (/ (reduce + (map #(+ (:sim %) (* 0.03 (dec (:weight %)))) top)) (count top)))
                       0.5)
           ;; only a strong resemblance to something disliked counts against
           penalty (if-let [n (first neg)] (* 2.0 (max 0.0 (- (:sim n) penalty-sim))) 0.0)]
       {:score (- pos-score penalty)
        :because (when (and (first pos) (> (:sim (first pos)) because-sim)) (first pos))
        :unlike (when (pos? penalty) (first neg))}))))

;;; --- centred model ----------------------------------------------------------

(defn- quantile [xs q]
  (let [v (vec (sort xs))] (v (min (dec (count v)) (long (* q (count v)))))))

(defn- sims-sample
  "Similarities between the examples and every 10th event."
  [examples vecs]
  (for [e examples, v (take-nth 10 vecs)] (embed/cosine (:vec e) v)))

(defn matched-sim
  "The similarity in centred space that's as selective as `raw-sim` is in raw
  space: the same quantile of example–event similarity."
  [raw-examples centred-examples raw-vecs center raw-sim]
  (let [raw (sims-sample raw-examples raw-vecs)]
    (if (empty? raw)
      raw-sim
      (quantile (sims-sample centred-examples (map center raw-vecs))
                (/ (count (filter #(< % raw-sim) raw)) (count raw))))))

(defn taste-model
  "Taste examples centred on the mean of `vecs` (the upcoming events)."
  [examples vecs]
  (let [mu (embed/mean-vec vecs)
        center (partial embed/center mu)
        centred (mapv #(update % :vec center) examples)]
    {:center center
     :examples centred
     :because-sim (matched-sim examples centred vecs center strong-sim)}))

(defn model-score
  "taste-score for raw vector `v` under a taste-model. No dislike penalty: after
  centring it hurt (bench, 2026-11-16)."
  [{:keys [center examples because-sim]} v]
  (when v
    (taste-score examples (center v) {:penalty-sim ##Inf :because-sim because-sim})))

(defn model
  "taste-model for the app: all taste examples, centred on the upcoming events."
  [ds examples]
  (taste-model examples (keep #(@store/vectors (:hash %)) (store/items ds))))

;;; --- what to ask about next ---------------------------------------------------

(defn- nearest [examples ^floats v pred]
  (reduce (fn [best e]
            (if (pred e)
              (let [s (embed/cosine (:vec e) v)]
                (if (or (nil? best) (> s (:sim best))) (assoc e :sim s) best))
              best))
          nil examples))

(defn- farthest-first
  "Greedily pick `n` of `cands` ([item centred-vec]), each time the one least
  like the examples and the picks so far."
  [examples cands n]
  (let [cands (vec cands)
        closest (double-array (for [[_ v] cands]
                                (reduce max -1.0 (map #(embed/cosine (:vec %) v) examples))))]
    (loop [picked []]
      (if (or (= n (count picked)) (= (count picked) (count cands)))
        (mapv cands picked)
        (let [i (apply min-key #(aget closest %) (remove (set picked) (range (count cands))))
              [_ vi] (cands i)]
          (dotimes [j (count cands)]
            (aset closest j (max (aget closest j) (embed/cosine vi (second (cands j))))))
          (recur (conj picked i)))))))

(defn- alternate
  "a1 b1 a2 b2 …, then whatever's left of the longer one."
  [a b]
  (lazy-seq (if (seq a) (cons (first a) (alternate b (rest a))) b)))

(defn learning-batch
  "Up to `n` of `items` whose ratings would teach the model most, each with a
  :learn-why. Half are confounding: ranked in the top fifth, but about as close
  to something you didn't go for (👎, 😐, 'not interested in…') as to something
  you did. Half are distinctive: unlike anything rated so far, and unlike each
  other (farthest-first), so the model explores rather than just refines."
  [{:keys [center examples] :as model} items n]
  (let [cands (vec (for [it items
                         :let [raw (@store/vectors (:hash it))]
                         :when raw]
                     (let [v (center raw)]
                       {:item it :v v
                        :score (:score (model-score model raw))
                        :pos (nearest examples v #(pos? (:weight %)))
                        :neg (nearest examples v #(and (not (pos? (:weight %))) (or (:key %) (neg? (:weight %)))))})))
        cutoff (when (seq cands) (nth (sort (map :score cands)) (long (* 0.8 (count cands)))))
        confounding (->> cands
                         (filter #(and (:pos %) (:neg %) (>= (:score %) cutoff)))
                         (sort-by #(- (:sim (:pos %)) (:sim (:neg %))))
                         ;; one per like/dislike pair: each is a different boundary to settle
                         (reduce (fn [acc c] (if (some #(= (map :label [(:pos c) (:neg c)])
                                                           (map :label [(:pos %) (:neg %)])) acc)
                                               acc (conj acc c)))
                                 [])
                         (take (quot n 2))
                         (map (fn [{:keys [item pos neg]}]
                                (assoc item :learn-why
                                       (str "Unsure: it's like " (:label pos) " (" (:why pos) ") but also "
                                            (:label neg) " (" (:why neg) ")."))))
                         vec)
        taken (set (map :key confounding))
        distinctive (->> cands
                         (remove #(taken (:key (:item %))))
                         ;; stubs are 'distinctive' only because there's nothing to them
                         (filter #(>= (count (str (:description (:item %)))) 200))
                         (map (juxt :item :v))
                         (#(farthest-first examples % (- n (count confounding))))
                         (map (fn [[item _]] (assoc item :learn-why "New territory: not much like anything you've rated yet."))))]
    ;; alternate, so a short session still sees both kinds
    (vec (alternate confounding distinctive))))

(defn- keyword-hit? [q item]
  (let [hay (str/lower-case (str/join " " (concat [(:title item) (:subtitle item) (:venue item)] (:tags item))))
        words (remove #(< (count %) 3) (str/split (str/lower-case q) #"\W+"))]
    (and (seq words) (every? #(str/includes? hay %) words))))

(defn score-items
  "Attach :rank (higher is better), :because and :qsim to each item.
  `model` is from (model ds examples). `query-vec` (optional) is a free-text
  query or 'more like this' vector."
  [ds items {:keys [model query-vec q]}]
  (for [item items
        :let [v (@store/vectors (:hash item))
              t (model-score model v)
              qsim (when (and query-vec v) (embed/cosine query-vec v))
              kw (when (and q (keyword-hit? q item)) 0.08)
              rank (cond
                     (nil? v) -1.0                       ; not embedded yet: last
                     qsim (+ (* 0.65 qsim) (* 0.35 (:score t)) (or kw 0))
                     :else (:score t))]]
    (assoc item :rank rank :because (:because t) :unlike (:unlike t) :qsim qsim)))
