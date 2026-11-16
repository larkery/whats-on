(ns whats-on.sources.we-the-curious
  "We The Curious. Only evening and special events are taken; day-visit
  activities and daily planetarium shows are part of normal admission. Event
  pages list their dates as 'Upcoming events: Nov 19 - 7:15pm, Dec 3 - 6pm …'."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.wethecurious.org")
(def wanted-types #{"Evening event" "Special event"})
(def max-pages 5)

(defn- card [c]
  {:url (u/abs-href-at c "a.card-link")
   :title (u/text-at c ".card-header-title")
   :summary (u/text-at c ".card-summary")
   :type (u/text-at c ".card-type")})

(defn- parse-occurrence
  "'Nov 19 - 7:15pm' (month first) or '21 November, 7pm' -> [LocalDate time]"
  [s]
  (let [date (or (first (u/parse-dates s))
                 (when-let [[_ m d] (re-find #"(?i)\b(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)[a-z]*\.?\s+(\d{1,2})\b" s)]
                   (u/infer-year (u/month-num m) (parse-long d))))]
    (when date [date (u/parse-time s)])))

(defn- occurrences [doc]
  (let [heading (first (filter #(re-find #"(?i)upcoming (events|dates)" (str (u/text %))) (u/sel doc "h2,h3,h4,h5,h6,p")))
        lis (some-> heading .nextElementSibling (u/sel "li"))]
    (keep #(parse-occurrence (u/text %)) lis)))

(defn- event-rows [{:keys [url title summary type]}]
  (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
        desc (or (u/paragraphs doc ".content > p" :min-len 60) summary)
        price-text (some->> (u/text doc) (re-find #"(?i)(?:tickets?|price)\s*:?\s*(£\d[\d.]*[^.]{0,40})") second)]
    (for [[date time] (occurrences doc)]
      (merge {:source-id (str (u/slug-id url) "@" date)
              :venue "We The Curious"
              :title title
              :subtitle summary
              :description desc
              :start-date (str date)
              :start-time time
              :price-text price-text
              :url url
              :booking-url url
              :category type
              :tags [type]
              :image (u/og-image doc)}
             (u/parse-prices price-text)))))

(defn events [{:keys [from to]}]
  (let [cards (loop [n 0 acc []]
                (let [doc (u/parse-html (http/fetch (str base "/whats-on/events" (when (pos? n) (str "?page=" n)))) base)
                      cs (map card (u/sel doc ".card"))
                      acc (into acc cs)]
                  (if (and (seq cs) (u/sel1 doc (str "a[href=?page=" (inc n) "]")) (< n max-pages))
                    (recur (inc n) acc)
                    acc)))]
    (->> cards
         (filter #(and (:url %) (wanted-types (:type %))))
         (mapcat #(try (event-rows %)
                       (catch Exception e
                         (u/log "  [we-the-curious] failed" (:url %) (ex-message e))
                         [])))
         (filter #(u/in-window? % from to)))))
