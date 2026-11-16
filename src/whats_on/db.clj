(ns whats-on.db
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [pod.babashka.go-sqlite3 :as sqlite]))

(def schema
  ["create table if not exists events (
      id           text primary key,  -- '<source>:<source_id>'
      source       text not null,
      source_id    text not null,
      venue        text,
      title        text not null,
      subtitle     text,
      description  text,
      start_date   text not null,     -- yyyy-mm-dd, UK local
      start_time   text,              -- HH:MM, null if unknown
      end_date     text,              -- set for runs (a show on over several days)
      end_time     text,
      date_text    text,              -- the site's own wording, e.g. 'Weds 17th & Thu 18th Feb'
      price_min    real,              -- GBP
      price_max    real,
      price_text   text,
      url          text,
      booking_url  text,
      category     text,
      tags         text,              -- JSON array
      image        text,
      sold_out     integer not null default 0,
      first_seen   text not null,
      last_seen    text not null,
      canonical_id text               -- id of the preferred listing when duplicated across sources
    )"
   "create index if not exists events_start on events(start_date)"
   "create index if not exists events_canonical on events(canonical_id)"
   "create table if not exists runs (
      source      text not null,
      started_at  text not null,
      finished_at text,
      ok          integer,
      n_events    integer,
      error       text
    )"
   ;; One row per real-world event, preferring venue listings over aggregators,
   ;; filling price/description gaps from duplicates, and listing other sources.
   "drop view if exists upcoming"
   "create view upcoming as
    select e.id, e.source, e.venue, e.title, e.subtitle,
           coalesce(e.description, (select d.description from events d
                                    where d.canonical_id = e.id and d.description is not null limit 1)) as description,
           e.start_date, e.start_time, e.end_date, e.end_time, e.date_text,
           coalesce(e.price_min, (select min(d.price_min) from events d where d.canonical_id = e.id)) as price_min,
           coalesce(e.price_max, (select max(d.price_max) from events d where d.canonical_id = e.id)) as price_max,
           e.price_text, e.url, e.booking_url, e.category, e.tags, e.image, e.sold_out,
           (select group_concat(d.source || ' ' || coalesce(d.booking_url, d.url), char(10))
              from events d where d.canonical_id = e.id and d.id <> e.id) as also_listed
      from events e
     where e.canonical_id = e.id
       and coalesce(e.end_date, e.start_date) >= date('now', 'localtime')"])

(def columns
  [:id :source :source_id :venue :title :subtitle :description
   :start_date :start_time :end_date :end_time :date_text
   :price_min :price_max :price_text :url :booking_url :category :tags :image
   :sold_out :first_seen :last_seen :canonical_id])

(defn init! [db]
  (doseq [s schema] (sqlite/execute! db [s])))

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

(defn upsert-source!
  "Replace `source`'s upcoming events with `events`. Past rows are kept as history;
  upcoming rows no longer listed (cancelled / removed) are deleted."
  [db source events now today]
  (let [rows (->> events
                  (filter #(and (:source-id %) (:title %) (:start-date %)))
                  (map #(->row source now %))
                  ;; a source can list the same thing twice; last one wins
                  (reduce #(assoc %1 (:id %2) %2) {})
                  vals)]
    ;; Each pod call is its own connection, so no transaction spans these;
    ;; batched multi-row statements keep it quick instead.
    (doseq [chunk (partition-all batch-size rows)]
      (sqlite/execute! db (into [(upsert-sql (count chunk))]
                                (mapcat (fn [r] (map r columns)) chunk))))
    (sqlite/execute! db ["delete from events where source = ? and last_seen <> ?
                          and coalesce(end_date, start_date) >= ?"
                         (name source) now (str today)])
    (count rows)))

(defn log-run! [db {:keys [source started-at finished-at ok n error]}]
  (sqlite/execute! db ["insert into runs (source, started_at, finished_at, ok, n_events, error) values (?,?,?,?,?,?)"
                       (name source) started-at finished-at (if ok 1 0) n error]))

(defn future-rows [db today]
  (sqlite/query db ["select id, source, venue, title, start_date, start_time, end_date, canonical_id,
                            description, price_min
                       from events where coalesce(end_date, start_date) >= ?" (str today)]))

(defn set-canonical!
  "Apply [id canonical-id] pairs, in batches."
  [db pairs]
  (doseq [chunk (partition-all 200 pairs)]
    (sqlite/execute! db (into [(str "with v(id, c) as (values "
                                    (str/join ", " (repeat (count chunk) "(?, ?)"))
                                    ") update events set canonical_id = (select c from v where v.id = events.id)"
                                    " where id in (select id from v)")]
                              (apply concat chunk)))))

(defn upcoming [db {:keys [to q category]}]
  (sqlite/query db (into [(str "select * from upcoming where start_date <= ?"
                               (when q " and (title like ? or subtitle like ? or description like ? or venue like ? or tags like ?)")
                               (when category " and (category like ? or tags like ?)")
                               " order by start_date, coalesce(start_time, '99'), venue, title")
                          (str to)]
                         (concat (when q (repeat 5 (str "%" q "%")))
                                 (when category (repeat 2 (str "%" category "%")))))))
