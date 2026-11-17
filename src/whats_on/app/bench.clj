(ns whats-on.app.bench
  "Leave-one-out testbench for the recommender (improvements.org, item 1).

    clojure -M:bench snapshot              freeze whats-on.db → bench/snapshots/<date>.db
    clojure -M:bench run [snapshot.db]     run every scorer, print a comparison and
                                           write per-item results to bench/results/

  Each labelled event (feedback) is held out in turn. The scorer is fitted on
  everything else, then the held-out event is ranked against the snapshot's
  frozen pool of upcoming events. A result is the event's percentile in that
  pool: 1.0 means it would come top, 0.5 means it's lost in the middle.

  Three modes: :plain holds out just the one event; :grouped also drops
  training examples that are near-duplicates of it (same title stem, or cosine
  > 0.9), so a repeat screening or a series can't just find its twin; :venue
  also drops everything at its venue, to see how much rides on venue."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [whats-on.app.embed :as embed]
            [whats-on.app.rank :as rank]
            [whats-on.app.store :as store])
  (:import [java.time LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util Random]))

;;; --- snapshots --------------------------------------------------------------

(def snapshot-dir "bench/snapshots")
(def results-dir "bench/results")

(defn snapshot!
  "Copy `src` to bench/snapshots/<today>.db, freeze its upcoming events into
  bench_pool (the upcoming view moves with the clock) and embed them all."
  [src]
  (let [date (str (LocalDate/now))
        out (io/file snapshot-dir (str date ".db"))]
    (when (.exists out) (throw (ex-info (str out " already exists") {})))
    (io/make-parents out)
    (jdbc/execute! (store/datasource src) [(str "vacuum into '" out "'")])
    (let [ds (store/datasource (str out))]
      (jdbc/execute! ds ["create table bench_pool as select * from upcoming"])
      (jdbc/execute! ds ["create table bench_meta (k text primary key, v text)"])
      (jdbc/execute! ds ["insert into bench_meta values ('snapshot_date', ?)" date])
      (store/load-vectors! ds)
      (let [pool (store/group-items (store/q ds ["select * from bench_pool"]))]
        (println "Pool:" (count pool) "events; embedding any missing…")
        (doseq [{:keys [text]} pool] (store/vector-for ds text))))
    (println "Wrote" (str out))
    (str out)))

(defn latest-snapshot []
  (->> (.listFiles (io/file snapshot-dir))
       (map #(.getPath ^java.io.File %))
       (filter #(re-find #"/\d{4}-\d{2}-\d{2}\.db$" %))
       sort last))

(defn- title-stem
  "Normalised title up to the first ':' or dash, so a series ('Relaxation Sound
  Therapy: …') or a billing variant ('LowKey: Sunda Arc + support') matches."
  [title]
  (-> (str/lower-case (str title))
      (str/split #"\s*(:|\s[-–—]\s)")
      first
      (str/replace #"[^a-z0-9]+" " ")
      str/trim))

(defn load-snapshot
  "→ {:name :examples :labelled :pool :pool-vecs}. Labelled events are the
  feedback rows that can be held out; bookings with no description are
  training-only (title + venue alone doesn't embed like an event)."
  [path]
  (let [ds (store/datasource path)
        _ (store/load-vectors! ds)
        bookings (store/q ds ["select title, venue, description from bookings"])
        sparse (let [by-key (group-by #(store/norm-key (:title %) (:venue %)) bookings)]
                 (set (for [[k rows] by-key :when (every? #(str/blank? (:description %)) rows)] k)))
        fb (remove #(str/blank? (:text %)) (store/feedback ds))
        fb-keys (set (map :key fb))
        examples (vec (for [e (rank/examples ds)]
                        (assoc e :sparse? (boolean (and (= "booked" (:verdict e)) (sparse (:key e)))))))
        labelled (vec (for [{:keys [key verdict title venue text]} fb
                            :when (not (and (= "booked" verdict) (sparse key)))]
                        {:key key :verdict verdict :title title :venue venue
                         :vec (store/vector-for ds text)}))
        pool (->> (store/group-items (store/q ds ["select * from bench_pool order by start_date, start_time"]))
                  ;; labelled events mustn't compete with themselves
                  (remove #(fb-keys (:key %)))
                  (mapv #(assoc (select-keys % [:key :title :venue]) :vec (store/vector-for ds (:text %)))))]
    {:name (str/replace (.getName (io/file path)) #"\.db$" "")
     :examples examples
     :labelled labelled
     :pool pool
     :pool-vecs (mapv :vec pool)}))

;;; --- scorers -----------------------------------------------------------------
;; A scorer is {:name :fit}, where (fit train-examples pool) → (fn [event] score).
;; Pool events, examples and held-out events are all maps with :vec and :venue.
;; Higher scores are better; only the order matters.

(defn- on-vecs
  "A scorer from `fit` over bare vectors: (fit train pool-vecs) → (fn [vec] score)."
  [name fit]
  {:name name
   :fit (fn [train pool] (let [f (fit train (mapv :vec pool))] (fn [x] (f (:vec x)))))})

(defn venue-key [venue] (str/trim (str/replace (str/lower-case (str venue)) #"[^a-z0-9]+" " ")))

(def baseline
  (on-vecs "baseline" (fn [train _pool] (fn [v] (:score (rank/taste-score train v))))))

(defn train-without
  "`scorer`, fitted without the training examples matching `pred`."
  [name pred scorer]
  {:name name :fit (fn [train pool] ((:fit scorer) (remove pred train) pool))})

(defn centered
  "Subtract the pool's mean vector from everything before comparing. bge's
  vectors share a big common component (unrelated texts sit at ~0.6 cosine);
  removing it should spread similarities out and weaken hubs.
  `threshold` is the dislike penalty's similarity: a number, or :matched to
  move it to the same quantile of example–event similarity that 0.62 is in raw
  space, so the penalty stays as selective as before."
  [name threshold]
  (on-vecs name
           (fn [train pool]
             (let [c (partial embed/center (embed/mean-vec pool))
                   train' (mapv #(update % :vec c) train)
                   penalty-sim (if (= :matched threshold)
                                 (rank/matched-sim train train' pool c rank/strong-sim)
                                 threshold)]
               (fn [v] (:score (rank/taste-score train' (c v) {:penalty-sim penalty-sim})))))))

(def no-penalty
  ;; a dislike never counts against an event
  ##Inf)

(def app
  ;; exactly what the app runs (rank/taste-model + rank/model-score)
  (on-vecs "app" (fn [train pool]
                   (let [m (rank/taste-model train pool)]
                     (fn [v] (:score (rank/model-score m v)))))))

(defn centerer
  "→ (fn [{:keys [vec venue]}] centred vec). With k nil, centre on the pool's
  mean, as the app does. With a number k, centre each event on its venue's mean
  shrunk toward the pool's: μ + α(μ_venue − μ), α = n/(n+k), n = the venue's
  pool events. A big venue loses most of its house style (boilerplate, the way
  its listings are written); a venue with a handful of events barely moves.
  k = 0 centres each venue fully. Examples at venues not in the pool, and
  statements, are centred on the pool mean."
  [pool k]
  (let [mu (embed/mean-vec (map :vec pool))
        shifts (when k
                 (into {} (for [[vk evs] (group-by (comp venue-key :venue) pool)
                                :let [n (count evs)
                                      a (/ n (+ n k))
                                      mu-v (embed/mean-vec (map :vec evs))]]
                            [vk (float-array (map #(+ %1 (* a (- %2 %1))) mu mu-v))])))]
    (fn [{:keys [vec venue]}]
      (embed/center (or (and venue (get shifts (venue-key venue))) mu) vec))))

(defn venue-centred
  "app's model (no dislike penalty), but on centerer's venue-shrunk centring."
  [name k]
  {:name name
   :fit (fn [train pool]
          (let [c (centerer pool k)
                train' (mapv #(assoc % :vec (c %)) train)]
            (fn [x] (:score (rank/taste-score train' (c x) {:penalty-sim no-penalty})))))})

(def scorers
  [app
   baseline
   (venue-centred "venue-shrunk k=20" 20)
   (venue-centred "venue-shrunk k=5" 5)
   (venue-centred "venue-centred fully" 0)])

(def penalty-ablations
  [baseline
   (on-vecs "baseline, no penalty"
            (fn [train _pool] (fn [v] (:score (rank/taste-score train v {:penalty-sim no-penalty})))))
   (centered "centered" :matched)
   app])

(def booking-ablations
  [baseline
   (train-without "baseline −booked" #(= "booked" (:verdict %)) baseline)
   (train-without "baseline −sparse booked" :sparse? baseline)])

;;; --- leave one out --------------------------------------------------------

(def top-fractions [0.03 0.10])   ; ~ what Plan shows per month; a looser cut

(defn- leaks?
  ":plain drops only the held-out event; :grouped also its near-duplicates;
  :venue also everything at its venue, so the model has to place it from what
  it is, not where it is."
  [mode held ex]
  (or (and (:key ex) (= (:key held) (:key ex)))
      (and (#{:grouped :venue} mode)
           (or (and (:title ex) (= (title-stem (:title held)) (title-stem (:title ex))))
               (> (embed/cosine (:vec held) (:vec ex)) 0.9)))
      (and (= :venue mode) (:venue ex) (= (venue-key (:venue held)) (venue-key (:venue ex))))))

(defn- fold [{:keys [examples pool]} {:keys [fit]} mode held]
  (let [train (vec (remove #(leaks? mode held %) examples))
        score (fit train pool)
        pool-scores (double-array (map score pool))
        s (double (score held))
        n (alength pool-scores)
        above (areduce pool-scores i acc 0 (if (> (aget pool-scores i) s) (inc acc) acc))
        ties (areduce pool-scores i acc 0 (if (== (aget pool-scores i) s) (inc acc) acc))
        nearest (apply max-key :sim (map #(assoc % :sim (embed/cosine (:vec held) (:vec %))) train))]
    (merge (select-keys held [:key :verdict :title :venue])
           {:pct (- 1.0 (/ (+ above (* 0.5 ties)) n))
            :rank (inc above)
            :nearest (:label nearest) :nearest-sim (:sim nearest) :nearest-why (:why nearest)
            :n-train (count train)})))

(defn loo
  "Hold out each labelled event in turn → one result map per event."
  [snap scorer mode]
  (vec (pmap #(fold snap scorer mode %) (:labelled snap))))

;;; --- metrics ----------------------------------------------------------------

(def positive? #{"like" "love" "booked"})

;; graded preference, for concordance
(def level {"love" 3 "booked" 3 "like" 2 "meh" 1 "dislike" 0})

(defn- mean [xs] (when (seq xs) (/ (reduce + xs) (count xs))))

(defn concordance
  "Over pairs of held-out events with different levels (⭐/✓ > 👍 > 😐 > 👎):
  the share the model orders the same way, ties counting half."
  [results]
  (let [rs (vec (filter #(level (:verdict %)) results))
        pairs (for [i (range (count rs)) j (range (inc i) (count rs))
                    :let [a (rs i) b (rs j)
                          d (compare (level (:verdict a)) (level (:verdict b)))]
                    :when (not (zero? d))]
                (let [p (compare (:pct a) (:pct b))]
                  (cond (zero? p) 0.5 (= p d) 1.0 :else 0.0)))]
    (mean pairs)))

(defn auc
  "P(a random positive outranks a random dislike), on LOO percentiles."
  [results]
  (let [pos (keep #(when (positive? (:verdict %)) (:pct %)) results)
        neg (keep #(when (= "dislike" (:verdict %)) (:pct %)) results)]
    (when (and (seq pos) (seq neg))
      (/ (reduce + (for [p pos d neg] (cond (> p d) 1.0 (= p d) 0.5 :else 0.0)))
         (* (count pos) (count neg))))))

(defn summary [results]
  (into {:auc (auc results) :concordance (concordance results)}
        (for [[label pred] [["positive" (comp positive? :verdict)]
                            ["love" #(= "love" (:verdict %))]
                            ["like" #(= "like" (:verdict %))]
                            ["booked" #(= "booked" (:verdict %))]
                            ["meh" #(= "meh" (:verdict %))]
                            ["dislike" #(= "dislike" (:verdict %))]]
              :let [pcts (map :pct (filter pred results))]
              :when (seq pcts)]
          [label (into {:n (count pcts) :mean-pct (mean pcts)}
                       (for [f top-fractions]
                         [(keyword (str "top" (Math/round (* 100 f))))
                          (double (/ (count (filter #(>= % (- 1.0 f)) pcts)) (count pcts)))]))])))

(defn paired
  "Compare scorer b against a on the same held-out events of one class:
  mean Δ percentile with a 95% bootstrap CI, and wins/losses (Δ > 0.005)."
  [results-a results-b pred]
  (let [by-key (into {} (map (juxt :key :pct)) results-a)
        ds (vec (for [r results-b :when (pred r)] (- (:pct r) (by-key (:key r)))))
        rng (Random. 1)
        n (count ds)
        boots (sort (repeatedly 2000 #(mean (repeatedly n (fn [] (ds (.nextInt rng n)))))))]
    (when (pos? n)
      {:n n :mean-delta (mean ds)
       :ci [(nth boots 50) (nth boots 1949)]
       :wins (count (filter #(> % 0.005) ds))
       :losses (count (filter #(< % -0.005) ds))})))

(defn hubs
  "Pool events that are among the k nearest neighbours of the largest share of
  positive examples: close to everything, so they float up whatever the taste."
  [{:keys [examples pool]} k n]
  (let [pos (filter #(pos? (:weight %)) examples)
        freq (frequencies
              (for [e pos
                    i (->> (range (count pool))
                           (sort-by #(- (embed/cosine (:vec e) (:vec (pool %)))))
                           (take k))]
                i))]
    (for [[i c] (take n (sort-by (comp - val) freq))]
      {:title (:title (pool i)) :venue (:venue (pool i))
       :share (double (/ c (count pos)))})))

(defn because-venue
  "Fitted on every example, centred by (centerer pool k): for the pool's top 3%,
  the share whose nearest positive example (what the app cites as 'because
  you liked…') is at the same venue, and that example's mean similarity; the
  same share over the whole pool for reference. Plus a few top suggestions
  with their 'because'."
  [{:keys [examples pool]} k]
  (let [c (centerer pool k)
        pos (filterv #(pos? (:weight %)) (map #(assoc % :vec (c %)) examples))
        scored (vec (for [x pool
                          :let [v (c x)
                                near (apply max-key :sim (map #(assoc % :sim (embed/cosine (:vec %) v)) pos))]]
                      {:x x :near near :same? (= (venue-key (:venue x)) (venue-key (:venue near)))
                       :score (:score (rank/taste-score pos v {:penalty-sim no-penalty}))}))
        top (take (long (Math/ceil (* 0.03 (count scored)))) (sort-by (comp - :score) scored))
        share #(double (/ (count (filter :same? %)) (count %)))]
    {:top-same (share top) :all-same (share scored)
     :top-sim (mean (map (comp :sim :near) top))
     :examples (for [{:keys [x near]} (take 6 top)]
                 (str (:title x) " · " (:venue x) "  ←  " (:label near) (format " (%.2f)" (:sim near))))}))

(defn k-occurrence-skew
  "Skewness of N_k: how often each vector is among the k nearest neighbours of
  the others. Near 0 is healthy; large and positive means a few hubs."
  [vecs k]
  (let [vecs (vec vecs)
        n (count vecs)
        counts (long-array n)]
    (doseq [i (range n)
            j (->> (range n) (remove #{i})
                   (sort-by #(- (embed/cosine (vecs i) (vecs %))))
                   (take k))]
      (aset counts j (inc (aget counts j))))
    (let [xs (map double counts)
          m (mean xs)
          sd (Math/sqrt (mean (map #(Math/pow (- % m) 2) xs)))]
      (/ (mean (map #(Math/pow (- % m) 3) xs)) (Math/pow sd 3)))))

;;; --- output -----------------------------------------------------------------

(defn- git-version []
  (let [sha (str/trim (:out (sh/sh "git" "rev-parse" "--short" "HEAD")))
        dirty (not (str/blank? (:out (sh/sh "git" "status" "--porcelain" "--" "src"))))]
    (str sha (when dirty "-dirty"))))

(defn- fmt-pct [x] (if x (format "%5.1f" (* 100.0 x)) "    -"))

(defn print-summaries [rows]
  (println (format "%-26s %-8s %5s %5s | %-17s | %-17s | %-17s | %-17s | %-8s"
                   "scorer" "mode" "AUC" "conc" "love pct top3 10" "like  pct top3 10" "booked pct top3 10"
                   "dislike pct top3 10" "meh pct"))
  (doseq [{:keys [scorer mode summary]} rows
          :let [{:strs [love like booked meh dislike]} summary
                cell (fn [{:keys [mean-pct top3 top10]}]
                       (str (fmt-pct mean-pct) " " (fmt-pct top3) " " (fmt-pct top10)))]]
    (println (format "%-26s %-8s %5s %5s | %-17s | %-17s | %-17s | %-17s | %-8s"
                     scorer (name mode) (fmt-pct (:auc summary)) (fmt-pct (:concordance summary))
                     (cell love) (cell like) (cell booked) (cell dislike) (fmt-pct (:mean-pct meh))))))

(defn- csv-cell [x]
  (let [s (str x)] (if (re-find #"[\",\n]" s) (str "\"" (str/replace s "\"" "\"\"") "\"") s)))

(defn write-results! [snap version rows]
  (let [stamp (.format (LocalDateTime/now) (DateTimeFormatter/ofPattern "yyyyMMdd-HHmmss"))
        out (io/file results-dir (str stamp "-" (:name snap) "-" version ".csv"))
        cols [:scorer :mode :key :verdict :title :venue :pct :rank :nearest :nearest-sim :nearest-why :n-train]]
    (io/make-parents out)
    (with-open [w (io/writer out)]
      (.write w (str (str/join "," (map name cols)) "\n"))
      (doseq [{:keys [scorer mode results]} rows
              r results]
        (.write w (str (str/join "," (map csv-cell (map (assoc r :scorer scorer :mode (name mode)) cols))) "\n"))))
    (str out)))

(defn run
  "Run `scorers` over a snapshot in both modes; prints and saves the results."
  ([path] (run path scorers))
  ([path scorers]
   (let [snap (load-snapshot path)
         version (git-version)
         _ (println (format "Snapshot %s · code %s · %d held out · %d training examples · pool %d"
                            (:name snap) version (count (:labelled snap))
                            (count (:examples snap)) (count (:pool snap))))
         rows (vec (for [mode [:plain :grouped :venue] s scorers
                         :let [results (loo snap s mode)]]
                     {:scorer (:name s) :mode mode :results results :summary (summary results)}))
         base (into {} (for [r rows :when (= (:scorer r) (:name (first scorers)))] [(:mode r) (:results r)]))]
     (println "\nPercentile in pool (100 = top); top3/top10 = % of held-out events in the pool's top 3%/10%")
     (print-summaries rows)
     (println (str "\nPaired against " (:name (first scorers)) " (Δ percentile points, 95% CI, wins/losses):"))
     (doseq [{:keys [scorer mode results]} rows
             :when (not= scorer (:name (first scorers)))
             [label pred] [["positives" (comp positive? :verdict)] ["dislikes" #(= "dislike" (:verdict %))]]
             :let [{:keys [n mean-delta ci wins losses]} (paired (base mode) results pred)]
             :when n]
       (println (format "  %-26s %-8s %-9s Δ %+5.1f  [%+5.1f, %+5.1f]  %d/%d of %d"
                        scorer (name mode) label (* 100 mean-delta)
                        (* 100 (first ci)) (* 100 (second ci)) wins losses n)))
     (println "\n'Because you liked…' at the same venue (top 3% of the pool vs all of it), and k-occurrence skew (k=10):")
     (doseq [[label k] [["pool mean (app)" nil] ["venue-shrunk k=20" 20] ["venue-shrunk k=5" 5] ["venue-centred fully" 0]]
             :let [{:keys [top-same all-same top-sim examples]} (because-venue snap k)
                   c (centerer (:pool snap) k)]]
       (println (format "  %-20s same venue: top %4.0f%%, all %4.0f%% · mean sim %.2f · skew %.2f"
                        label (* 100 top-same) (* 100 all-same) top-sim
                        (k-occurrence-skew (map c (:pool snap)) 10)))
       (doseq [e examples] (println "      " e)))
     (println "\nPer-item results:" (write-results! snap version rows))
     rows)))

(defn -main [& [cmd arg]]
  (case cmd
    "snapshot" (snapshot! (or arg "whats-on.db"))
    "run" (run (or arg (latest-snapshot) (throw (ex-info "No snapshot; run `snapshot` first" {}))))
    (println "usage: clojure -M:bench snapshot [db] | run [snapshot.db]"))
  (shutdown-agents))
