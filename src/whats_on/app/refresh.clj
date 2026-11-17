(ns whats-on.app.refresh
  "Fetch events from every source into the db, in the background, keeping its
  progress in `state` for the Refresh page. Also runs on its own, e.g. from
  cron: `clojure -M:scrape [--only a,b] [--days N]`."
  (:require [babashka.cli :as cli]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [whats-on.app.store :as store]
            [whats-on.dedupe :as dedupe]
            [whats-on.http :as http]
            [whats-on.sources :as sources]
            [whats-on.util :as u])
  (:import [java.time LocalDate LocalTime]
           [java.time.format DateTimeFormatter]
           [java.time.temporal ChronoUnit]))

(def default-config
  {:days 183   ; ~6 months, so "Plan ahead" has something to plan
   :delay-ms 4000
   ;; Optional: appended to the User-Agent so site owners can reach you,
   ;; e.g. "mailto:you@example.org"
   :contact nil
   ;; e.g. #{:hippodrome} to skip sources
   :disabled #{}})

;;; Listings like "Highlights Tour: 2 April 2024 – 31 December 2027" or
;;; permanent displays aren't events; drop anything spanning longer than this.
(def max-span-days 180)

(defn- too-long? [{:keys [start-date end-date]}]
  (and end-date
       (> (.between ChronoUnit/DAYS (LocalDate/parse (str start-date)) (LocalDate/parse (str end-date)))
          max-span-days)))

(defn source-names
  "The sources a refresh will fetch, in name order."
  [cfg]
  (->> (keys sources/registry) (remove (set (:disabled cfg))) (sort-by name)))

;;; --- progress -------------------------------------------------------------

;;; {:running? :started-ms :finished-ms :from :to
;;;  :sources {k {:status :waiting|:fetching|:saving|:done|:failed
;;;               :n :error :started-ms :finished-ms}}
;;;  :phase "…" :dupes n :log ["HH:mm:ss line" …]}
(defonce state (atom {:running? false}))

(def ^:private max-log-lines 500)

(defn- log-line! [line]
  (let [t (.format (LocalTime/now u/london) (DateTimeFormatter/ofPattern "HH:mm:ss"))]
    (swap! state update :log
           (fn [log] (let [log (conj (or log []) (str t " " line))]
                       (cond-> log (> (count log) max-log-lines) (subvec 1)))))))

(defn- source! [k & kvs] (swap! state update-in [:sources k] #(apply assoc % kvs)))

;;; --- writing ----------------------------------------------------------------

(def ^:private columns
  [:id :source :source_id :venue :title :subtitle :description
   :start_date :start_time :end_date :end_time :date_text
   :price_min :price_max :price_text :url :booking_url :category :tags :image
   :sold_out :first_seen :last_seen :canonical_id])

(defn- ->row [source now e]
  {:id (str (name source) ":" (:source-id e))
   :source (name source)
   :source_id (str (:source-id e))
   :venue (:venue e)
   :title (:title e)
   :subtitle (:subtitle e)
   :description (:description e)
   :start_date (str (:start-date e))
   :start_time (:start-time e)
   :end_date (some-> (:end-date e) str)
   :end_time (:end-time e)
   :date_text (:date-text e)
   :price_min (:price-min e)
   :price_max (:price-max e)
   :price_text (:price-text e)
   :url (:url e)
   :booking_url (:booking-url e)
   :category (:category e)
   :tags (json/generate-string (vec (distinct (remove str/blank? (:tags e)))))
   :image (:image e)
   :sold_out (if (:sold-out? e) 1 0)
   :first_seen now
   :last_seen now
   :canonical_id nil})

(def ^:private batch-size 50)

(defn- upsert-sql
  "A multi-row upsert for n rows (keeps first_seen and canonical_id on update)."
  [n]
  (str "insert into events (" (str/join ", " (map name columns)) ") values "
       (str/join ", " (repeat n (str "(" (str/join ", " (repeat (count columns) "?")) ")")))
       " on conflict(id) do update set "
       (->> columns
            (remove #{:id :first_seen :canonical_id})
            (map #(str (name %) " = excluded." (name %)))
            (str/join ", "))))

;; sources finish in parallel; let them take turns at the db rather than
;; wait on sqlite's busy timeout
(defonce ^:private write-lock (Object.))

(defn- upsert-source!
  "Replace `source`'s upcoming events with `events`. Past rows are kept as history;
  upcoming rows no longer listed (cancelled / removed) are deleted."
  [ds source events now today]
  (let [rows (->> events
                  (filter #(and (:source-id %) (:title %) (:start-date %)))
                  (map #(->row source now %))
                  ;; a source can list the same thing twice; last one wins
                  (reduce #(assoc %1 (:id %2) %2) {})
                  vals)]
    (locking write-lock
      (jdbc/with-transaction [tx ds]
        (doseq [chunk (partition-all batch-size rows)]
          (jdbc/execute! tx (into [(upsert-sql (count chunk))]
                                  (mapcat (fn [r] (map r columns)) chunk))))
        (jdbc/execute! tx ["delete from events where source = ? and last_seen <> ?
                            and coalesce(end_date, start_date) >= ?"
                           (name source) now (str today)])))
    (count rows)))

(defn- log-run! [ds {:keys [source started-at finished-at ok n error]}]
  (locking write-lock
    (jdbc/execute! ds ["insert into runs (source, started_at, finished_at, ok, n_events, error) values (?,?,?,?,?,?)"
                       (name source) started-at finished-at (if ok 1 0) n error])))

(defn- dedupe! [ds today]
  (let [rows (store/q ds ["select id, source, venue, title, start_date, start_time, end_date, canonical_id,
                                  description, price_min
                             from events where coalesce(end_date, start_date) >= ?" (str today)])
        priority (into {} (map (fn [[k v]] [(name k) (:priority v 10)])) sources/registry)
        patterns (keep (fn [[k v]] (when (:venue v) [k (:venue v)])) sources/registry)
        canon (dedupe/canonical-ids rows priority patterns)
        changed (filter (fn [r] (not= (:canonical_id r) (canon (:id r)))) rows)]
    (locking write-lock
      (jdbc/with-transaction [tx ds]
        (doseq [chunk (partition-all 200 changed)]
          (jdbc/execute! tx (into [(str "with v(id, c) as (values "
                                        (str/join ", " (repeat (count chunk) "(?, ?)"))
                                        ") update events set canonical_id = (select c from v where v.id = events.id)"
                                        " where id in (select id from v)")]
                                  (mapcat (juxt :id #(canon (:id %))) chunk))))))
    (count (filter (fn [[id c]] (not= id c)) canon))))

;;; --- running ------------------------------------------------------------------

(defn- run-source [ds k {:keys [from to today]}]
  (let [started (store/now)]
    (source! k :status :fetching :started-ms (System/currentTimeMillis))
    (u/log (str "[" (name k) "] fetching…"))
    (let [result (try
                   (let [evs (doall (remove too-long? ((get-in sources/registry [k :fn]) {:from from :to to})))]
                     (u/log (format "[%s] %d events" (name k) (count evs)))
                     (source! k :status :saving)
                     {:ok true :n (upsert-source! ds k evs (store/now) today)})
                   (catch Exception e
                     (u/log (format "[%s] FAILED: %s" (name k) (ex-message e)))
                     {:ok false :error (ex-message e)}))]
      (log-run! ds (assoc result :source k :started-at started :finished-at (store/now)))
      (source! k :status (if (:ok result) :done :failed) :n (:n result) :error (:error result)
               :finished-ms (System/currentTimeMillis))
      (assoc result :source k))))

(defn- fetch-all!
  "Fetch `selected` sources (in parallel; politeness is per host, inside
  http/fetch), save each as it finishes, then match duplicates. Blocks.
  Returns the per-source results."
  [ds cfg selected]
  (let [today (u/today)
        to (.plusDays today (long (:days cfg)))]
    (swap! http/config merge (select-keys cfg [:delay-ms :contact]))
    (http/reset-run!)
    (swap! state assoc :from today :to to :phase "Fetching")
    (u/log (format "Fetching %s → %s from %d sources (≥%.1fs between requests to the same site)"
                   today to (count selected) (/ (:delay-ms cfg) 1000.0)))
    (let [results (->> selected
                       (mapv #(future (run-source ds % {:from today :to to :today today})))
                       (mapv deref))]
      (swap! state assoc :phase "Matching duplicates")
      (let [dupes (dedupe! ds today)]
        (swap! state assoc :dupes dupes)
        (u/log (format "Done. %d listings matched as duplicates." dupes)))
      (when-let [failed (seq (remove :ok results))]
        (u/log "Failed sources:" (str/join ", " (map (comp name :source) failed))))
      results)))

(defn- begin! [selected]
  (let [[old _] (swap-vals! state
                            (fn [s]
                              (if (:running? s)
                                s
                                {:running? true
                                 :started-ms (System/currentTimeMillis)
                                 :phase "Starting"
                                 :sources (into (sorted-map-by #(compare (name %1) (name %2)))
                                                (map (fn [k] [k {:status :waiting}]))
                                                selected)
                                 :log []})))]
    (not (:running? old))))

(defn- finish! [phase]
  (swap! state assoc :running? false :finished-ms (System/currentTimeMillis) :phase phase))

(defn- go! [ds cfg selected]
  (binding [u/*sink* log-line!]
    (try (fetch-all! ds cfg selected)
         (finish! "Done")
         (catch Throwable e
           (u/log "Refresh failed:" (ex-message e))
           (finish! "Failed")
           (throw e)))))

(defn start!
  "Refresh in the background, unless one is already running. `only` limits it
  to those sources; `on-done` is called after a refresh that finishes.
  Returns true if this started one."
  [ds cfg {:keys [only on-done]}]
  (let [cfg (merge default-config cfg)
        selected (cond->> (source-names cfg) (seq only) (filter (set only)))]
    (when (and (seq selected) (begin! selected))
      (future
        (try (go! ds cfg selected)
             (when on-done (on-done))
             (catch Throwable e (.printStackTrace e))))
      true)))

;;; --- command line -------------------------------------------------------------

(defn -main [& args]
  (let [opts (cli/parse-opts args {:coerce {:days :long :only :string}})
        cfg (merge default-config
                   {:db "whats-on.db"}
                   (when (.exists (io/file "config.edn")) (edn/read-string (slurp "config.edn")))
                   (select-keys opts [:days]))
        only (some->> (:only opts) (#(str/split % #",")) (map keyword) set)
        selected (cond->> (source-names cfg) only (filter only))
        ds (store/datasource (:db cfg))]
    (begin! selected)
    (go! ds cfg selected)
    (u/log "Requests per site (network / cached / not-modified):")
    (doseq [[host {:keys [requests cached not-modified]}] (sort @http/stats)]
      (u/log (format "  %-32s %4d / %4d / %4d" host (or requests 0) (or cached 0) (or not-modified 0))))
    (shutdown-agents)
    (System/exit (if (every? #(= :done (:status %)) (vals (:sources @state))) 0 1))))
