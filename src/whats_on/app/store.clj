(ns whats-on.app.store
  "Database access for the app. Reads events written by the bb scrapers, and
  owns the taste tables: feedback, statements, history, plus cached embeddings."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [whats-on.app.embed :as embed])
  (:import [java.security MessageDigest]
           [java.time Instant]))

(def schema
  ["pragma journal_mode = wal"
   "create table if not exists embeddings (
      hash text primary key,       -- sha1 of the embedded text
      model text not null,
      vec blob not null,
      created_at text not null)"
   ;; one verdict per event 'key' (normalised title + venue), so a verdict on
   ;; a film covers all its screenings, and survives after the event has passed
   "create table if not exists feedback (
      key text primary key,
      verdict text not null,       -- dislike | meh | like | love | booked
      title text, venue text, start_date text, url text,
      text text,                   -- the text that was embedded, kept for retraining
      text_hash text,
      updated_at text not null)"
   "create table if not exists statements (
      id integer primary key,
      text text not null,
      weight real not null,        -- +1 'I like…', -1 'not interested in…'
      created_at text not null)"
   "create table if not exists history (
      id integer primary key,
      title text not null,
      venue text, date text, notes text,
      created_at text not null)"
   ;; 'don't show me this', separate from taste: hiding says nothing about
   ;; whether you'd like it (seen it, can't go, …)
   "create table if not exists hidden (
      key text primary key,
      title text, venue text,
      created_at text not null)"
   ;; hiding used to be a feedback verdict
   "insert or ignore into hidden (key, title, venue, created_at)
      select key, title, venue, updated_at from feedback where verdict = 'hidden'"
   "delete from feedback where verdict = 'hidden'"])

(defn sha1 [s]
  (let [d (.digest (MessageDigest/getInstance "SHA-1") (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) d))))

(defn now [] (str (Instant/now)))

(defn datasource [db-path]
  (let [ds (jdbc/get-datasource {:dbtype "sqlite" :dbname db-path})]
    ;; wait rather than fail if the scraper is mid-write
    (jdbc/execute! ds ["pragma busy_timeout = 10000"])
    (doseq [s schema] (jdbc/execute! ds [s]))
    ds))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn q [ds sqlv] (jdbc/execute! ds sqlv opts))
(defn q1 [ds sqlv] (jdbc/execute-one! ds sqlv opts))

;;; --- events -------------------------------------------------------------

(defn norm-key
  "Identity for feedback: same title at the same venue = same thing."
  [title venue]
  (str (str/replace (str/lower-case (str title)) #"[^a-z0-9]+" " ")
       "|"
       (str/replace (str/lower-case (str venue)) #"[^a-z0-9]+" " ")))

(defn embed-text
  "What we embed for an event: the descriptive bits, not dates/prices/links."
  [{:keys [title subtitle category tags venue description]}]
  (let [tags (try (json/parse-string (or tags "[]")) (catch Exception _ []))]
    (->> [title subtitle
          (str/join ", " (distinct (remove str/blank? (cons category tags))))
          (some->> venue (str "at "))
          (some-> description (subs 0 (min 900 (count description))))]
         (remove str/blank?)
         (str/join ". "))))

(defn- ->item
  "Group the rows (performances/screenings) of one event into one item."
  [rows]
  (let [first-row (first rows)
        text (embed-text first-row)]
    (assoc first-row
           :key (norm-key (:title first-row) (:venue first-row))
           :text text
           :hash (sha1 text)
           :dates (vec (sort-by (juxt :start_date :start_time)
                                (map #(select-keys % [:id :start_date :start_time :end_date :booking_url :sold_out]) rows)))
           :tags (try (vec (json/parse-string (or (:tags first-row) "[]"))) (catch Exception _ [])))))

(defn group-items
  "Event rows (one per performance) → items, one per distinct title+venue."
  [rows]
  (->> rows
       (group-by #(norm-key (:title %) (:venue %)))
       vals
       (mapv ->item)))

(defonce ^:private cache (atom nil))

(defn- events-version [ds]
  (:v (q1 ds ["select coalesce(max(last_seen), '') || count(*) as v from events"])))

(defn items
  "All upcoming events, one item per distinct event (title+venue), each with
  its :dates. Cached until the scraper writes new data."
  [ds]
  (let [v (events-version ds)]
    (if (= v (:version @cache))
      (:items @cache)
      (let [items (group-items (q ds ["select * from upcoming order by start_date, start_time"]))]
        (reset! cache {:version v :items items})
        items))))

;;; --- embeddings ---------------------------------------------------------

(defonce vectors (atom {}))   ; hash -> float[]

(defn load-vectors! [ds]
  (reset! vectors
          (into {} (map (juxt :hash #(embed/<-bytes (:vec %))))
                (q ds ["select hash, vec from embeddings where model = ?" embed/model-name]))))

(defn vector-for
  "Embedding for `text`, computing and caching it if needed."
  [ds text]
  (let [h (sha1 text)]
    (or (@vectors h)
        (let [v (embed/embed text)]
          (jdbc/execute! ds ["insert or replace into embeddings (hash, model, vec, created_at) values (?,?,?,?)"
                             h embed/model-name (embed/->bytes v) (now)])
          (swap! vectors assoc h v)
          v))))

;;; --- taste --------------------------------------------------------------

(defn feedback [ds]
  (q ds ["select * from feedback order by updated_at desc"]))

(defn feedback-map [ds]
  (into {} (map (juxt :key :verdict)) (feedback ds)))

(defn set-feedback! [ds item verdict]
  (if (or (nil? verdict) (= "none" verdict))
    (jdbc/execute! ds ["delete from feedback where key = ?" (:key item)])
    (jdbc/execute! ds ["insert into feedback (key, verdict, title, venue, start_date, url, text, text_hash, updated_at)
                        values (?,?,?,?,?,?,?,?,?)
                        on conflict(key) do update set verdict = excluded.verdict, updated_at = excluded.updated_at"
                       (:key item) verdict (:title item) (:venue item) (:start_date item)
                       (:url item) (:text item) (:hash item) (now)])))

(defn hidden [ds] (q ds ["select * from hidden order by created_at desc"]))

(defn hidden-keys [ds] (set (map :key (hidden ds))))

(defn set-hidden! [ds item hidden?]
  (if hidden?
    (jdbc/execute! ds ["insert or ignore into hidden (key, title, venue, created_at) values (?,?,?,?)"
                       (:key item) (:title item) (:venue item) (now)])
    (jdbc/execute! ds ["delete from hidden where key = ?" (:key item)])))

(defn statements [ds] (q ds ["select * from statements order by created_at desc"]))

(defn add-statement! [ds text weight]
  (jdbc/execute! ds ["insert into statements (text, weight, created_at) values (?,?,?)" text weight (now)]))

(defn delete-statement! [ds id]
  (jdbc/execute! ds ["delete from statements where id = ?" id]))

(defn history [ds] (q ds ["select * from history order by date desc, created_at desc"]))

(defn add-history! [ds {:keys [title venue date notes]}]
  (jdbc/execute! ds ["insert into history (title, venue, date, notes, created_at) values (?,?,?,?,?)"
                     title venue date notes (now)]))

(defn delete-history! [ds id]
  (jdbc/execute! ds ["delete from history where id = ?" id]))

(defn import-bookings!
  "One-off: seed feedback with the past bookings extracted from email (see
  whats-on.bookings) as 'booked'. Bookings have the same descriptive columns as
  events, so their text is embedded the same way. Repeat bookings of the same
  thing collapse into one row; an existing verdict is never overwritten.
  Returns the number of rows added."
  [ds]
  (let [groups (group-by #(norm-key (:title %) (:venue %))
                         (q ds ["select * from bookings order by event_date"]))]
    (reduce + (for [[k rows] groups
                    ;; prefer a row with a description, then the most recent
                    :let [b (or (last (remove #(str/blank? (:description %)) rows)) (last rows))
                          text (embed-text b)]]
                (:next.jdbc/update-count
                 (jdbc/execute-one! ds ["insert into feedback (key, verdict, title, venue, start_date, url, text, text_hash, updated_at)
                                         values (?,?,?,?,?,?,?,?,?)
                                         on conflict(key) do nothing"
                                        k "booked" (:title b) (:venue b) (:event_date b) (:url b)
                                        text (sha1 text) (now)]))))))
