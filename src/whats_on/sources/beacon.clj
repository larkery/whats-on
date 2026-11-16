(ns whats-on.sources.beacon
  "Bristol Beacon. The listing can be filtered by a date range and is paged;
  each card is one performance with date, time, genre and room. Event pages
  add a description and price range."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://bristolbeacon.org")
(def max-pages 30)

(defn- listing-page [from to n]
  (let [url (str base "/whats-on/" (when (> n 1) (str "page/" n "/")) "?dates=" from "%7C" to)
        doc (u/parse-html (http/fetch url) base)]
    (for [c (u/sel doc ".c-event-card")
          :let [date (first (u/parse-dates (u/text-at c ".c-event-card__meta-label--date")))]
          :when date]
      (let [genres (->> (u/sel c ".c-event-card__genre") (keep u/text) distinct vec)
            room (u/text-at c ".c-event-card__venue")
            booking (u/abs-href-at c "a.c-book-btn")
            message (u/text-at c ".c-event-card__message")]
        {:source-id (or (second (re-find #"/book/(\d+)" (str booking)))
                        (str (u/slug-id (u/abs-href-at c "a.c-col-card__link")) "@" date))
         :venue "Bristol Beacon"
         :title (u/text-at c ".c-event-card__title")
         :subtitle (->> [(u/text-at c ".c-event-card__pretitle") (u/text-at c ".c-event-card__subtitle")]
                        (remove nil?) (str/join " — ") u/clean)
         :start-date (str date)
         :start-time (u/parse-time (u/text-at c ".c-event-card__meta-label--time"))
         :url (u/abs-href-at c "a.c-col-card__link")
         :booking-url booking
         :category (first genres)
         :tags (vec (distinct (concat genres (when room [room]) (when message [message]))))
         :sold-out? (boolean (some->> message (re-find #"(?i)sold out")))}))))

(defn- cards [from to]
  (loop [n 1 acc []]
    (let [page (listing-page from to n)
          acc (into acc page)]
      (if (or (empty? page) (>= n max-pages))
        acc
        (recur (inc n) acc)))))

(defn- details [url]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
          ;; "Book Tickets £37.05 - £48.45 Incl. booking fee" (the header basket also shows a £ amount)
          price-text (some->> (u/text doc)
                              (re-find #"(?i)book tickets\s+(£\d[\d.]*(?:\s*-\s*£\d[\d.]*)?(?:\s*incl\. booking fee)?|free)")
                              second)]
      ;; the meta description is the site-wide tagline, so use the intro block
      (merge {:description (or (u/paragraphs doc ".c-event-introduction p")
                               (u/text-at doc ".c-event-introduction"))
              :image (u/og-image doc)
              :price-text price-text}
             (u/parse-prices price-text)))
    (catch Exception e
      (u/log "  [beacon] details failed" url (ex-message e))
      {})))

(defn events [{:keys [from to]}]
  (let [cs (cards from to)
        ;; several performances often share one event page
        info (into {} (map (juxt identity details)) (distinct (keep :url cs)))]
    (u/log (format "  [beacon] %d performances, %d event pages" (count cs) (count info)))
    (for [c cs]
      (merge c (info (:url c))))))
