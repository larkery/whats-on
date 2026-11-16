(ns whats-on.sources.arnos-vale
  "Arnos Vale Cemetery. Events are blog posts in an 'events' category; the date
  is in prose next to the title ('from 26 November to 28 December 2026')."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://arnosvale.org.uk")

(defn- ->event [c]
  (let [url (u/abs-href-at c ".entry-title a")
        date-text (u/text-at c ".ag_event_date")
        [start end] (u/parse-dates date-text)
        full-title (u/text-at c ".entry-title")
        excerpt (u/text-at c ".post-excerpt")
        ;; excerpts start with a type label, e.g. "TOUR: ..." or "SPECIAL EVENT: ..."
        kind (some->> excerpt (re-find #"^([A-Z][A-Z &]+):") second str/capitalize)
        tags (->> (.classNames c)
                  (keep #(second (re-matches #"tag-(.*)" %)))
                  (remove #{"bristol" "events" "things-to-do"})
                  (map #(str/replace % "-" " ")))]
    (when (and url start)
      {:source-id (u/slug-id url)
       :venue "Arnos Vale"
       :title (u/clean (str/replace (str full-title) (str date-text) ""))
       :description (some-> excerpt (str/replace #"^[A-Z][A-Z &]+:\s*" "") u/clean)
       :start-date (str start)
       :end-date (when (and end (not= end start)) (str end))
       :start-time (u/parse-time date-text)
       :date-text date-text
       :url url
       :booking-url url
       :category kind
       :tags (vec (distinct (remove nil? (cons kind tags))))
       :image (u/attr (u/sel1 c "noscript img, img[data-src]") "src")})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/events/")) base)]
    (->> (u/sel doc ".post-item.category-events")
         (keep ->event)
         (filter #(u/in-window? % from to)))))
