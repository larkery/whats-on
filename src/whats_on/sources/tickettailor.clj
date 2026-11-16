(ns whats-on.sources.tickettailor
  "TicketTailor box office pages (tickettailor.com/events/<slug>), used by
  several small venues. Server-rendered; each event page has prices in JSON-LD.
  TicketTailor's robots.txt only disallows checkout and widget URLs."
  (:require [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.tickettailor.com")

(defn- ->event [c {:keys [venue category]}]
  (let [url (u/abs-href-at c "a.event__link")
        when-text (u/text-at c ".event-meta__date")
        ;; "Tue 17 Nov 2026 7:30 PM - Sat 21 Nov 2026 GMT" or "Sun 15 Nov 2026 3:00 PM - 7:00 PM GMT"
        [start end] (u/parse-dates when-text)
        [t1 t2] (map second (re-seq #"(?i)(\d{1,2}:\d{2}\s*[AP]M)" (or when-text "")))]
    (when (and url start)
      {:source-id (or (second (re-find #"/(\d+)$" url)) (u/slug-id url))
       :venue (or venue (u/text-at c ".event-meta__location"))
       :title (u/text-at c ".event__title")
       :start-date (str start)
       :start-time (u/parse-time t1)
       :end-date (when (and end (not= end start)) (str end))
       :end-time (when-not end (u/parse-time t2))
       :date-text when-text
       :url url
       :booking-url url
       :category category
       :image (u/attr (u/sel1 c "img") "src")
       :sold-out? (boolean (re-find #"(?i)sold out" (str (u/text c))))})))

(defn- with-details [{:keys [url] :as e}]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
          ld (first (u/jsonld-events doc))
          offers (let [o (:offers ld)] (if (sequential? o) o (when o [o])))
          prices (keep #(some-> (or (:price %) (:lowPrice %)) str parse-double) offers)
          desc (or (u/html->text (some-> (u/sel1 doc ".event-description, .event__description, [class*=description]") .html))
                   (u/clean (:description ld))
                   (u/meta-description doc))]
      (cond-> (assoc e :description desc)
        (seq prices) (assoc :price-min (apply min prices) :price-max (apply max prices))))
    (catch Exception ex
      (u/log "  [tickettailor] details failed" url (ex-message ex))
      e)))

(defn events
  "Events from the box office page for `slug`. :venue overrides the per-event
  location text; :category is applied to everything."
  [slug {:keys [from to] :as opts}]
  (let [doc (u/parse-html (http/fetch (str base "/events/" slug)) base)]
    (->> (u/sel doc ".events-listing__item")
         (keep #(->event % opts))
         (filter #(u/in-window? % from to))
         (map with-details))))
