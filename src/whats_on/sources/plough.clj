(ns whats-on.sources.plough
  "The Plough, Easton: a pub with gigs. A Squarespace events page that lists
  each upcoming event in full (date, start/end time, description), so no event
  pages are fetched. Its robots.txt disallows Squarespace's ?format=json."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.plougheaston.co.uk")

(defn- ->event [a]
  (let [url (u/abs-href-at a "a.eventlist-title-link")
        date (u/attr (u/sel1 a "time.event-date") "datetime")]
    (when (and url date)
      {:source-id (last (str/split (u/slug-id url) #"/"))
       :venue "The Plough, Easton"
       :title (some-> (u/text-at a ".eventlist-title") (str/replace #"(?<!\.)\.$" ""))
       :description (some->> (u/sel a ".eventlist-description .sqs-html-content")
                             (keep #(u/html->text (.html %)))
                             seq
                             (str/join "\n\n"))
       :start-date date
       :start-time (u/parse-time (u/text-at a ".event-time-localized-start"))
       :end-time (u/parse-time (u/text-at a ".event-time-localized-end"))
       :url url
       :image (u/attr (u/sel1 a ".eventlist-thumbnail") "data-src")})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/whats-on")) base)]
    (->> (u/sel doc "article.eventlist-event--upcoming")
         (keep ->event)
         (filter #(u/in-window? % from to)))))
