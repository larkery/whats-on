(ns whats-on.main
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [whats-on.db :as db]
            [whats-on.dedupe :as dedupe]
            [whats-on.http :as http]
            [whats-on.util :as u]
            [whats-on.sources.alma :as alma]
            [whats-on.sources.arnolfini :as arnolfini]
            [whats-on.sources.arnos-vale :as arnos-vale]
            [whats-on.sources.beacon :as beacon]
            [whats-on.sources.bristol-cathedral :as bristol-cathedral]
            [whats-on.sources.bristol-museums :as bristol-museums]
            [whats-on.sources.cube :as cube]
            [whats-on.sources.headfirst :as headfirst]
            [whats-on.sources.hen-and-chicken :as hen-and-chicken]
            [whats-on.sources.hippodrome :as hippodrome]
            [whats-on.sources.lost-in-the-reads :as lost-in-the-reads]
            [whats-on.sources.megascreen :as megascreen]
            [whats-on.sources.old-vic :as old-vic]
            [whats-on.sources.redgrave :as redgrave]
            [whats-on.sources.st-georges :as st-georges]
            [whats-on.sources.st-mary-redcliffe :as st-mary-redcliffe]
            [whats-on.sources.storysmith :as storysmith]
            [whats-on.sources.tobacco-factory :as tobacco-factory]
            [whats-on.sources.trinity :as trinity]
            [whats-on.sources.wardrobe :as wardrobe]
            [whats-on.sources.watershed :as watershed]
            [whats-on.sources.we-the-curious :as we-the-curious])
  (:import [java.time Instant LocalDate]
           [java.time.temporal ChronoUnit]))

;;; Each source: a fetch fn, and a pattern matching how *other* sites (i.e.
;;; Headfirst) write its venue name, used for de-duplication. Patterns are
;;; matched against lower-cased names with '&' spelled 'and' and apostrophes
;;; removed. Lower priority numbers win when the same event is listed twice.
(def registry
  {:watershed         {:fn watershed/events         :venue #"watershed"}
   :tobacco-factory   {:fn tobacco-factory/events   :venue #"tobacco factory"}
   :old-vic           {:fn old-vic/events           :venue #"old vic"}
   :hippodrome        {:fn hippodrome/events        :venue #"hippodrome"}
   :alma              {:fn alma/events              :venue #"\balma\b"}
   :wardrobe          {:fn wardrobe/events          :venue #"wardrobe"}
   :st-georges        {:fn st-georges/events        :venue #"st\.? georges? bristol|^st\.? georges?$"}
   :trinity           {:fn trinity/events           :venue #"trinity cent"}
   :beacon            {:fn beacon/events            :venue #"beacon|colston hall"}
   :cube              {:fn cube/events              :venue #"\bcube\b"}
   :redgrave          {:fn redgrave/events          :venue #"redgrave"}
   :arnolfini         {:fn arnolfini/events         :venue #"arnolfini"}
   :bristol-museums   {:fn bristol-museums/events   :venue #"m shed|bristol museum|blaise castle|georgian house|red lodge"}
   :arnos-vale        {:fn arnos-vale/events        :venue #"arnos vale"}
   :we-the-curious    {:fn we-the-curious/events    :venue #"we the curious"}
   :bristol-cathedral {:fn bristol-cathedral/events :venue #"bristol cathedral"}
   :st-mary-redcliffe {:fn st-mary-redcliffe/events :venue #"st\.? mary redcliffe"}
   :hen-and-chicken   {:fn hen-and-chicken/events   :venue #"hen\s+and\s+chicken"}
   :storysmith        {:fn storysmith/events        :venue #"storysmith"}
   :lost-in-the-reads {:fn lost-in-the-reads/events :venue #"lost in the reads"}
   :megascreen        {:fn megascreen/events        :venue #"megascreen"}
   :headfirst         {:fn headfirst/events         :priority 20}})

;;; Listings like "Highlights Tour: 2 April 2024 – 31 December 2027" or
;;; permanent displays aren't events; drop anything spanning longer than this.
(def max-span-days 180)

(defn- too-long? [{:keys [start-date end-date]}]
  (and end-date
       (> (.between ChronoUnit/DAYS (LocalDate/parse (str start-date)) (LocalDate/parse (str end-date)))
          max-span-days)))

(def default-config
  {:db "whats-on.db"
   :days 183   ; ~6 months, so "Plan ahead" has something to plan
   :delay-ms 4000
   ;; Optional: appended to the User-Agent so site owners can reach you,
   ;; e.g. "mailto:you@example.org"
   :contact nil
   ;; e.g. #{:hippodrome} to skip sources
   :disabled #{}})

(defn- load-config []
  (merge default-config
         (when (fs/exists? "config.edn") (edn/read-string (slurp "config.edn")))))

(defn- now-str [] (str (Instant/now)))

(defn- run-source [k {:keys [from to]}]
  (let [started (now-str)]
    (u/log (str "[" (name k) "] fetching…"))
    (try
      (let [evs (doall (remove too-long? ((get-in registry [k :fn]) {:from from :to to})))]
        (u/log (format "[%s] %d events" (name k) (count evs)))
        {:source k :events evs :ok true :started-at started})
      (catch Exception e
        (u/log (format "[%s] FAILED: %s" (name k) (ex-message e)))
        {:source k :ok false :error (ex-message e) :started-at started}))))

(defn- dedupe! [dbf today]
  (let [rows (db/future-rows dbf today)
        priority (into {} (map (fn [[k v]] [(name k) (:priority v 10)])) registry)
        patterns (keep (fn [[k v]] (when (:venue v) [k (:venue v)])) registry)
        canon (dedupe/canonical-ids rows priority patterns)
        changed (filter (fn [r] (not= (:canonical_id r) (canon (:id r)))) rows)]
    (db/set-canonical! dbf (map (juxt :id #(canon (:id %))) changed))
    (count (filter (fn [[id c]] (not= id c)) canon))))

(defn scrape [& args]
  (let [opts (cli/parse-opts args {:coerce {:days :long :only :string}})
        cfg (load-config)
        today (u/today)
        from today
        to (.plusDays today (long (or (:days opts) (:days cfg))))
        only (some->> (:only opts) (#(str/split % #",")) (map keyword) set)
        selected (->> (keys registry)
                      (remove (set (:disabled cfg)))
                      (filter #(or (nil? only) (only %))))
        dbf (:db cfg)]
    (swap! http/config merge (select-keys cfg [:delay-ms :contact]))
    (db/init! dbf)
    (u/log (format "Fetching %s → %s from %d sources (≥%.1fs between requests to the same site)"
                   from to (count selected) (/ (:delay-ms cfg) 1000.0)))
    ;; sources run in parallel; politeness is per host, inside http/fetch
    (let [results (->> selected
                       (mapv #(future (run-source % {:from from :to to})))
                       (mapv deref))
          now (now-str)]
      (doseq [{:keys [source events ok error started-at]} results]
        (let [n (when ok (db/upsert-source! dbf source events now today))]
          (db/log-run! dbf {:source source :started-at started-at :finished-at now
                            :ok ok :n n :error error})))
      (let [dupes (dedupe! dbf today)]
        (u/log (format "\nDone. %d listings matched as duplicates." dupes)))
      (u/log "Requests per site (network / cached / not-modified):")
      (doseq [[host {:keys [requests cached not-modified]}] (sort @http/stats)]
        (u/log (format "  %-32s %4d / %4d / %4d" host (or requests 0) (or cached 0) (or not-modified 0))))
      (when-let [failed (seq (remove :ok results))]
        (u/log "Failed sources:" (str/join ", " (map (comp name :source) failed)))
        (System/exit 1)))))

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

(defn -main [& args]
  (apply scrape args))
