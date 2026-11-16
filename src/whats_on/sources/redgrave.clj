(ns whats-on.sources.redgrave
  "Redgrave Theatre, Clifton (Webflow). The listing has every show with a date
  range; show pages add the opening time, prices and a description."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://redgravetheatre.com")

(defn- ->event [c]
  (let [url (u/abs-href-at c "a[href^=/shows/]")
        dates (keep #(first (u/parse-dates (u/text %))) (u/sel c ".event-time-wrapp p"))
        [start end] [(first dates) (last dates)]]
    (when (and url start)
      {:source-id (u/slug-id url)
       :venue "Redgrave Theatre"
       :title (u/text-at c "h3")
       :start-date (str start)
       :end-date (when (and end (not= end start)) (str end))
       :url url
       :booking-url url
       :category "Theatre"
       :sold-out? (boolean (re-find #"(?i)sold out" (str (u/text c))))})))

(defn- with-details [{:keys [url] :as e}]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
          ;; "Nov 21, 2026 7:00 PM" in the show header
          opening (u/text-at doc "h5.director-title")
          price-text (->> (u/sel doc "p")
                          (keep u/text)
                          (filter #(re-find #"£\d" %))
                          (remove #(re-find #"(?i)booking fee" %))
                          (str/join " | ")
                          u/clean)
          ;; per-performance lines like "Thursday 26th November | 1pm & 7pm"
          schedule (->> (u/sel doc "p")
                        (keep u/text)
                        (filter #(re-matches #"(?i)[a-z]+day \d{1,2}(st|nd|rd|th)? [a-z]+ \|.*" %)))]
      (merge e
             {:start-time (u/parse-time opening)
              :description (u/meta-description doc)
              :price-text price-text
              :date-text (when (seq schedule) (str/join "; " schedule))
              :image (u/og-image doc)}
             (u/parse-prices price-text)))
    (catch Exception ex
      (u/log "  [redgrave] details failed" url (ex-message ex))
      e)))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/whats-on")) base)]
    (->> (u/sel doc ".event-card")
         (keep ->event)
         (filter #(u/in-window? % from to))
         (map with-details))))
