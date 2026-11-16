(ns whats-on.sources.arnolfini
  "Arnolfini. One listing of cards (exhibitions, performances, film, talks) with
  a category and a date or date range; event pages add a description and price."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://arnolfini.org.uk")

(defn- ->event [c]
  (let [url (u/abs-href-at c "h2 a")
        date-text (u/text-at c ".card__content p")
        [start end] (u/parse-dates date-text)
        category (u/text-at c ".category-link")]
    (when (and url start)
      {:source-id (u/slug-id url)
       :venue "Arnolfini"
       :title (u/text-at c "h2")
       :start-date (str start)
       :end-date (when (and end (not= end start)) (str end))
       :start-time (u/parse-time date-text)
       :date-text date-text
       :url url
       :booking-url url
       :category category
       :tags (some-> category vector)})))

(defn- with-details [{:keys [url] :as e}]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
          ;; e.g. "31 October 2026 - 31 January 2027 Gallery 5, Level 2 FREE, donations welcome"
          info (->> (u/sel doc "main p, article p")
                    (keep u/text)
                    (filter #(re-find u/date-re %))
                    first)
          price-text (when info (some->> (re-find #"(?i)(free.*|£.*)$" info) first u/clean))]
      (merge e
             {:description (or (u/paragraphs doc "main p, article p" :min-len 80)
                               (u/meta-description doc))
              :price-text price-text
              ;; for runs (exhibitions, courses) a time on the page is opening hours
              :start-time (or (:start-time e) (when-not (:end-date e) (u/parse-time info)))
              :image (u/og-image doc)}
             (u/parse-prices price-text)))
    (catch Exception ex
      (u/log "  [arnolfini] details failed" url (ex-message ex))
      e)))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/whats-on/")) base)]
    (->> (u/sel doc ".card.type-event")
         (keep ->event)
         ;; teaching packs etc. are listed alongside events
         (remove #(= "Resources" (:category %)))
         (filter #(u/in-window? % from to))
         (map with-details))))
