(ns whats-on.sources.cube
  "The Cube cinema. One programme page with a card per showing. Dates have
  no year (\"Sun 15 November // 18:00\"), so the year is inferred."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.cubecinema.com")

(defn- ->event [c]
  (let [url (u/abs-href-at c "a[href*=/programme/event/]")
        when-text (u/text-at c ".start_and_pricing")
        date (first (u/parse-dates when-text))
        tags (->> (u/sel c ".tags a") (keep u/text) vec)
        ;; anything after the time is pricing / status, e.g. "£8/£6" or "(SOLD OUT)"
        extra (some-> when-text (str/replace #"^.*?//\s*\d{1,2}[:.]\d{2}" "") u/clean)]
    (when (and url date)
      (merge
       {:source-id (or (second (re-find #"event_(\d+)" (str (.id c)))) (u/slug-id url))
        :venue "The Cube"
        :title (u/text-at c "h3")
        :subtitle (->> [(u/text-at c ".pre_title") (u/text-at c ".post_title")] (remove nil?) (str/join " ") u/clean)
        :description (some-> (u/text-at c ".copy") (str/replace #"\s*…?\s*\[more\]\s*$" "…") u/clean)
        :start-date (str date)
        :start-time (u/parse-time (second (str/split when-text #"//")))
        :price-text extra
        :url url
        :booking-url url
        :category (some-> (first (filter #{"film" "music" "performance" "comedy" "theatre" "talk" "workshop" "club"} tags))
                          str/capitalize)
        :tags tags
        :image (u/abs-url (u/sel1 c ".event_image img") "src")
        :sold-out? (boolean (some->> extra (re-find #"(?i)sold out")))}
       (u/parse-prices extra)))))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/programme/")) base)]
    (->> (u/sel doc ".showing")
         (keep ->event)
         (filter #(u/in-window? % from to)))))
