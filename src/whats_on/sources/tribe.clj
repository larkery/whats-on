(ns whats-on.sources.tribe
  "WordPress sites running The Events Calendar ('tribe') expose a JSON API at
  /wp-json/tribe/events/v1/events. Used by Bristol Cathedral and the Hen & Chicken."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def per-page 50)

(defn- ->event [e {:keys [venue]}]
  (let [[sd st] (str/split (str (:start_date e)) #" ")
        [ed et] (str/split (str (:end_date e)) #" ")
        all-day? (:all_day e)
        cats (map (comp u/decode-entities :name) (:categories e))
        cost (u/decode-entities (:cost e))]
    (merge
     {:source-id (str (:id e))
      :venue (or venue (u/decode-entities (get-in e [:venue :venue])))
      :title (u/decode-entities (:title e))
      :description (u/html->text (:description e))
      :start-date sd
      :start-time (when-not all-day? (some-> st (subs 0 5)))
      :end-date (when (and ed (not= ed sd)) ed)
      :end-time (when-not all-day? (some-> et (subs 0 5)))
      :price-text cost
      :url (:url e)
      :booking-url (or (u/clean (:website e)) (:url e))
      :category (first cats)
      :tags (vec (distinct (concat cats (map (comp u/decode-entities :name) (:tags e)))))
      :image (get-in e [:image :url])}
     (u/parse-prices cost))))

(defn events
  "All events between from and to on a tribe site. `keep?` filters the raw API
  events (they carry :categories, :title etc)."
  [base {:keys [from to keep?] :or {keep? (constantly true)} :as opts}]
  (loop [page 1 acc []]
    (let [resp (http/fetch-json (http/url (str base "/wp-json/tribe/events/v1/events")
                                          [[:start_date (str from)] [:end_date (str to " 23:59:59")]
                                           [:per_page per-page] [:page page]]))
          evs (:events resp)
          acc (into acc (comp (filter keep?) (map #(->event % opts))) evs)]
      (if (and (seq evs) (< page (or (:total_pages resp) 1)))
        (recur (inc page) acc)
        acc))))

(defn category-names [e]
  (set (map (comp str/lower-case u/decode-entities :name) (:categories e))))
