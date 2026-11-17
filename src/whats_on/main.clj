(ns whats-on.main
  "The bb tasks that read the db. Fetching lives in the app: whats-on.app.refresh."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [whats-on.db :as db]
            [whats-on.sources :refer [registry]]
            [whats-on.util :as u]))

(defn- load-config []
  (merge {:db "whats-on.db"}
         (when (fs/exists? "config.edn") (edn/read-string (slurp "config.edn")))))

;;; --- output -------------------------------------------------------------

(defn- price-str [{:keys [price_min price_max price_text]}]
  (cond
    (and price_min (zero? price_min) (or (nil? price_max) (zero? price_max))) "Free"
    (and price_min price_max (not= price_min price_max)) (format "£%.2f–£%.2f" price_min price_max)
    price_min (format "£%.2f" price_min)
    :else price_text))

(defn- when-str [{:keys [start_time end_date date_text]}]
  (str/join " " (remove nil? [start_time
                              (when end_date (str "(runs until " end_date
                                                  (when date_text (str "; " date_text)) ")"))])))

(defn- fetch-upcoming [args]
  (let [opts (cli/parse-opts args {:coerce {:days :long}})
        cfg (load-config)
        to (.plusDays (u/today) (long (or (:days opts) 7)))]
    [opts (db/upcoming (:db cfg) {:to to :q (:q opts) :category (:category opts)})]))

(defn- day-heading [d]
  (let [ld (java.time.LocalDate/parse d)]
    (.format ld (java.time.format.DateTimeFormatter/ofPattern "EEEE d MMMM yyyy"))))

(defn upcoming
  "Plain text: one heading per day, one short block per event."
  [& args]
  (let [[_ rows] (fetch-upcoming args)
        today (str (u/today))]
    (doseq [[d evs] (sort-by key (group-by #(if (neg? (compare (:start_date %) today)) today (:start_date %)) rows))]
      (println)
      (println (day-heading d))
      (println (apply str (repeat (count (day-heading d)) "=")))
      (doseq [e evs]
        (println)
        (println (str (some-> (:start_time e) (str "  ")) (:title e)
                      (when (= 1 (:sold_out e)) "  [SOLD OUT]")))
        (println (str "  " (str/join " · " (remove str/blank? [(:venue e) (:category e) (price-str e)]))))
        (when (:end_date e) (println (str "  On until " (:end_date e))))
        (when (:subtitle e) (println (str "  " (:subtitle e))))
        (println (str "  " (or (:booking_url e) (:url e))))))))

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn html
  "A deliberately plain HTML page: no colours, no images, large readable text."
  [& args]
  (let [[opts rows] (fetch-upcoming args)
        out (or (:out opts) "whats-on.html")
        today (str (u/today))
        body (str/join
              "\n"
              (for [[d evs] (sort-by key (group-by #(if (neg? (compare (:start_date %) today)) today (:start_date %)) rows))]
                (str "<h2>" (esc (day-heading d)) "</h2>\n"
                     (str/join
                      "\n"
                      (for [e evs]
                        (str "<article>\n<h3>" (some-> (:start_time e) esc (str " — "))
                             "<a href=\"" (esc (or (:booking_url e) (:url e))) "\">" (esc (:title e)) "</a>"
                             (when (= 1 (:sold_out e)) " (sold out)") "</h3>\n"
                             "<p>" (esc (str/join " · " (remove str/blank? [(:venue e) (:category e) (price-str e)
                                                                           (when (:end_date e) (str "on until " (:end_date e)))])))
                             "</p>\n"
                             (when (:subtitle e) (str "<p>" (esc (:subtitle e)) "</p>\n"))
                             (when-let [desc (:description e)]
                               (str "<details><summary>More</summary>"
                                    (str/join (for [p (str/split desc #"\n\n+")] (str "<p>" (esc p) "</p>")))
                                    (let [tags (seq (json/parse-string (or (:tags e) "[]")))]
                                      (when tags (str "<p>Tags: " (esc (str/join ", " tags)) "</p>")))
                                    "</details>\n"))
                             "</article>"))))))]
    (spit out (str "<!doctype html>\n<html lang=\"en-GB\"><head><meta charset=\"utf-8\">"
                   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
                   "<title>What's on</title>"
                   "<style>body{font:1.15rem/1.6 system-ui,sans-serif;max-width:40rem;margin:0 auto;padding:1rem}"
                   "h2{margin-top:2.5rem;border-bottom:1px solid}h3{margin-bottom:.2rem}p{margin:.2rem 0}"
                   "article{margin-bottom:1.2rem}</style></head><body>\n"
                   "<h1>What's on</h1>\n" body "\n</body></html>\n"))
    (u/log (str "Wrote " out " (" (count rows) " events)"))))

(defn sources [& _]
  (doseq [k (sort (keys registry))] (println (name k))))
