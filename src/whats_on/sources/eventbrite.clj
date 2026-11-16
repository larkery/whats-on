(ns whats-on.sources.eventbrite
  "Eventbrite collection pages (eventbrite.co.uk/cc/<name>-<id>) carry a JSON-LD
  ItemList of Events. (Organiser pages load their list with JavaScript, so
  collections are the usable route.) Collections include past events; the date
  window filters them out."
  (:require [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.eventbrite.co.uk")

(defn- ->event [ev {:keys [venue category]}]
  (let [[date time] (u/iso->local (some-> (:startDate ev) (clojure.string/replace #"([+-]\d{2})(\d{2})$" "$1:$2")))
        [end-date end-time] (u/iso->local (some-> (:endDate ev) (clojure.string/replace #"([+-]\d{2})(\d{2})$" "$1:$2")))
        url (:url ev)]
    (when (and date url)
      {:source-id (or (second (re-find #"-(\d+)$" url)) (u/slug-id url))
       :venue (or venue (get-in ev [:location :name]))
       :title (u/decode-entities (:name ev))
       :description (u/clean (:description ev))
       :start-date date
       :start-time time
       :end-date (when (and end-date (not= end-date date)) end-date)
       :end-time end-time
       :url url
       :booking-url url
       :category category
       :image (:image ev)})))

(defn events
  "Events from the given collection paths, e.g. [\"/cc/live-events-1931509\"]."
  [collections {:keys [from to] :as opts}]
  (->> collections
       (mapcat (fn [path]
                 (let [doc (u/parse-html (http/fetch (str base path)) base)]
                   (->> (u/jsonld doc)
                        (mapcat :itemListElement)
                        (keep :item)))))
       (keep #(->event % opts))
       (filter #(u/in-window? % from to))))
