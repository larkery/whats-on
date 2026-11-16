(ns whats-on.dedupe
  "Spot the same event listed by more than one source (mostly Headfirst vs the
  venue's own site). Two listings match when they're at the same venue, on a
  compatible date/time, with similar titles. Each row gets a canonical_id:
  itself, or the higher-priority listing it duplicates."
  (:require [clojure.set :as set]
            [clojure.string :as str])
  (:import [java.text Normalizer Normalizer$Form]))

(def ^:private stopwords
  #{"the" "a" "an" "and" "of" "at" "in" "on" "with" "plus" "presents" "present"
    "live" "tour" "bristol" "uk" "special" "guests" "guest" "support" "feat" "ft"
    "show" "night" "event" "tickets"})

(defn- fold [s]
  (-> (Normalizer/normalize (str s) Normalizer$Form/NFD)
      (str/replace #"\p{M}" "")
      str/lower-case
      (str/replace #"&" " and ")
      (str/replace #"'" "")))

(defn title-tokens [s]
  (->> (str/split (fold s) #"[^a-z0-9]+")
       (remove str/blank?)
       (remove stopwords)
       set))

(defn similar-titles? [a b]
  (let [ta (title-tokens a) tb (title-tokens b)]
    (and (seq ta) (seq tb)
         (or (set/subset? ta tb)
             (set/subset? tb ta)
             (>= (/ (count (set/intersection ta tb))
                    (count (set/union ta tb)))
                 1/2)))))

(defn venue-key
  "Map a free-text venue name onto a source's venue where we know it, so that
  e.g. Headfirst's 'The Trinity Centre' lines up with the trinity source."
  [venue-patterns {:keys [source venue]}]
  (let [v (fold venue)]
    (or (some (fn [[k re]] (when (re-find re v) k)) venue-patterns)
        (keyword source)
        (str/replace v #"[^a-z0-9]+" " "))))

(defn- minutes [t]
  (when-let [[_ h m] (and t (re-find #"^(\d{1,2}):(\d{2})" t))]
    (+ (* 60 (parse-long h)) (parse-long m))))

(defn- times-compatible?
  "Unknown times match anything; otherwise allow for doors-vs-start differences."
  [a b]
  (let [ma (minutes a) mb (minutes b)]
    (or (nil? ma) (nil? mb) (<= (abs (- ma mb)) 90))))

(defn- dates-compatible? [a b]
  (let [within? (fn [d {:keys [start_date end_date]}]
                  (and (<= (compare start_date d) 0)
                       (<= (compare d (or end_date start_date)) 0)))]
    (cond
      (and (:end_date a) (:end_date b)) (and (<= (compare (:start_date a) (:end_date b)) 0)
                                             (<= (compare (:start_date b) (:end_date a)) 0))
      (:end_date a) (within? (:start_date b) a)
      (:end_date b) (within? (:start_date a) b)
      :else (and (= (:start_date a) (:start_date b))
                 (times-compatible? (:start_time a) (:start_time b))))))

(defn- time-distance [a b]
  (let [ma (minutes (:start_time a)) mb (minutes (:start_time b))]
    (if (and ma mb) (abs (- ma mb)) 45)))

(defn canonical-ids
  "rows: maps with :id :source :venue :title :start_date :start_time :end_date.
  priority: source name -> number, lower is preferred.
  Returns {id canonical-id} for every row."
  [rows priority venue-patterns]
  (let [rows (map #(assoc % :vkey (venue-key venue-patterns %)) rows)
        prio #(priority (:source %) 50)
        by-venue (group-by :vkey rows)]
    (into {}
          (for [[_ group] by-venue
                r group
                :let [better (->> group
                                  (filter #(and (not= (:source %) (:source r))
                                                (< (prio %) (prio r))
                                                (dates-compatible? r %)
                                                (similar-titles? (:title r) (:title %))))
                                  (sort-by (juxt prio #(time-distance r %))))]]
            [(:id r) (:id (first better) (:id r))]))))
