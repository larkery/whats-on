(ns whats-on.sources.hippodrome
  "Bristol Hippodrome (ATG). One listing page of touring shows with date ranges.
  Class names are generated, so we go by structure rather than CSS classes.
  Descriptions come from each show page's meta description."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.atgtickets.com")
(def listing (str base "/venues/bristol-hippodrome/whats-on/"))

(defn- ->event [c]
  (let [url (u/abs-href-at c "a[href^=/shows/]")
        ps (keep u/text (u/sel c "p"))
        date-text (first (filter #(re-find u/date-re %) ps))
        dates (u/parse-dates date-text)
        until? (some->> date-text (re-find #"(?i)^until"))
        [start end] (if until? [(u/today) (first dates)] dates)
        category (first (remove #(or (= % date-text) (re-find #"(?i)hippodrome|sold out" %)) ps))]
    (when (and url start)
      {:source-id (u/slug-id url)
       :venue "Bristol Hippodrome"
       :title (u/text (last (u/sel c "h2")))
       :start-date (str start)
       :end-date (when (and end (not= end start)) (str end))
       :date-text date-text
       :url url
       :booking-url (or (u/abs-href-at c "a[href*=/calendar/]") url)
       :category category
       :tags (some-> category (str/split #",\s*") vec)
       :sold-out? (boolean (some #(re-find #"(?i)sold out" %) (cons (u/text c) ps)))})))

(defn- with-details [{:keys [url] :as e}]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)]
      (assoc e
             :description (u/meta-description doc)
             :image (u/og-image doc)))
    (catch Exception ex
      (u/log "  [hippodrome] details failed" url (ex-message ex))
      e)))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch listing) base)]
    (->> (u/sel doc "[data-testid=showCard]")
         (keep ->event)
         (filter #(u/in-window? % from to))
         (map with-details))))
