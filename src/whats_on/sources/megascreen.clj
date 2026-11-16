(ns whats-on.sources.megascreen
  "Bristol Megascreen (the former IMAX at Bristol Aquarium): festivals and special
  screenings. One 'now showing' page; tickets are sold through Headfirst, so
  these normally de-duplicate against Headfirst rows, adding fuller descriptions."
  (:require [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.bristolaquarium.co.uk")

(defn- ->event [c]
  (let [title (u/text-at c ".screening-title")
        when-text (some-> (u/text-at c ".screening-showtime") (clojure.string/replace #"(?i)^show\s*time\s*" ""))
        date (first (u/parse-dates when-text))
        booking (u/abs-href-at c ".screening-link a")]
    (when (and title date)
      {:source-id (or (second (re-find #"#e(\d+)" (str booking))) (str (u/slug-id title) "@" date))
       :venue "Bristol Megascreen"
       :title title
       :description (u/paragraphs c ".screening-description p")
       :start-date (str date)
       :start-time (u/parse-time when-text)
       :date-text when-text
       :tags (keep u/clean [(some-> (u/text-at c ".screening-runtime") (clojure.string/replace #"(?i)^runtime\s*" "Runtime "))])
       :url (str base "/megascreen-now-showing/")
       :booking-url booking
       :category "Film"
       :image (u/attr (u/sel1 c ".screening-image img") "src")})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/megascreen-now-showing/")) base)]
    (->> (u/sel doc ".screening-item")
         (keep ->event)
         (filter #(u/in-window? % from to)))))
