(ns whats-on.app.rank
  "Turning items + taste into ranked suggestions.

  Taste is a set of examples, each an embedding with a weight:
    👍 liked  +1 · ✓ booked +1.5 · past history +1.5 · 'I like…' statements +1
    👎 disliked −1 · 'not interested in…' statements −1
  An event scores by how close it is to its nearest positive examples, with a
  penalty if it's very close to something negative. Using the nearest few
  examples (not an average of everything) lets very different tastes coexist —
  liking both folk gigs and history talks doesn't blur into 'folky history'."
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

(def verdict-weights {"like" 1.0 "booked" 1.5 "dislike" -1.0})

(defn examples
  "All taste examples as {:vec :weight :label}. Computes any missing embeddings."
  [ds]
  (concat
   (for [{:keys [verdict text title venue]} (store/feedback ds)
         :let [w (verdict-weights verdict)]
         :when (and w (not (str/blank? text)))]
     {:vec (store/vector-for ds text) :weight w
      :label (str title (when venue (str " at " venue)))
      :why (if (neg? w) "you disliked" "you liked")})
   (for [{:keys [text weight]} (store/statements ds)]
     ;; statements read like queries ("I love folk music"), so use bge's query prefix
     {:vec (store/vector-for ds (str embed/query-prefix text)) :weight weight
      :label (str "“" text "”") :why "you said"})
   (for [{:keys [title venue notes]} (store/history ds)]
     {:vec (store/vector-for ds (str/join ". " (remove str/blank? [title (some->> venue (str "at ")) notes])))
      :weight 1.5
      :label (str title (when venue (str " at " venue))) :why "you went to"})))

(def ^:private top-k 3)

(defn taste-score
  "→ {:score s :because example} or nil if the item has no embedding yet."
  [examples ^floats v]
  (when v
    (let [sims (map #(assoc % :sim (embed/cosine (:vec %) v)) examples)
          pos (->> sims (filter #(pos? (:weight %))) (sort-by :sim >))
          neg (->> sims (filter #(neg? (:weight %))) (sort-by :sim >))
          pos-score (if (seq pos)
                      (let [top (take top-k pos)]
                        (/ (reduce + (map #(+ (:sim %) (* 0.03 (dec (:weight %)))) top)) (count top)))
                      0.5)
          ;; only a strong resemblance to something disliked counts against
          penalty (if-let [n (first neg)] (* 2.0 (max 0.0 (- (:sim n) 0.62))) 0.0)]
      {:score (- pos-score penalty)
       :because (when (and (first pos) (> (:sim (first pos)) 0.62)) (first pos))
       :unlike (when (pos? penalty) (first neg))})))

(defn- keyword-hit? [q item]
  (let [hay (str/lower-case (str/join " " (concat [(:title item) (:subtitle item) (:venue item)] (:tags item))))
        words (remove #(< (count %) 3) (str/split (str/lower-case q) #"\W+"))]
    (and (seq words) (every? #(str/includes? hay %) words))))

(defn score-items
  "Attach :rank (higher is better), :because and :qsim to each item.
  `query-vec` (optional) is a free-text query or 'more like this' vector."
  [ds items {:keys [examples query-vec q]}]
  (for [item items
        :let [v (@store/vectors (:hash item))
              t (taste-score examples v)
              qsim (when (and query-vec v) (embed/cosine query-vec v))
              kw (when (and q (keyword-hit? q item)) 0.08)
              rank (cond
                     (nil? v) -1.0                       ; not embedded yet: last
                     qsim (+ (* 0.65 qsim) (* 0.35 (:score t)) (or kw 0))
                     :else (:score t))]]
    (assoc item :rank rank :because (:because t) :unlike (:unlike t) :qsim qsim)))
