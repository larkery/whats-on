(ns whats-on.sources.tobacco-factory
  "Tobacco Factory Theatres. One listing page; each show has a date range, and
  its CSS classes double as tags (family, christmas, comedy...)."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://tobaccofactorytheatres.com")

(def ^:private not-tags #{"block" "show"})

(defn- ->event [c]
  (let [url (u/abs-href-at c "a.inside")
        date-text (u/text-at c ".excerpt span")
        [start end] (u/parse-dates date-text)
        tags (->> (.classNames c) (remove not-tags) (map #(str/replace % "-" " ")) vec)]
    (when (and url start)
      {:source-id (u/slug-id url)
       :venue "Tobacco Factory Theatres"
       :title (u/text-at c "h3")
       :subtitle (u/text-at c ".detail")
       :description (u/text-at c ".excerpt p")
       :start-date (str start)
       :end-date (when (and end (not= end start)) (str end))
       :date-text date-text
       :url url
       :booking-url (u/abs-href-at c "a.book-now")
       :category (some-> (first tags) str/capitalize)
       :tags tags
       :image (u/attr (u/sel1 c "img") "src")})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/whats-on/")) base)]
    (->> (u/sel doc ".block.show")
         (keep ->event)
         (filter #(u/in-window? % from to)))))
