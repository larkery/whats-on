(ns whats-on.sources.wardrobe
  "The Wardrobe Theatre. The listing page is built in JavaScript, but the
  WordPress REST API exposes the 'shows' post type with structured fields
  (dates, show time, price, age guidance)."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u])
  (:import [java.time LocalDate]
           [java.time.format DateTimeFormatter]))

(def base "https://thewardrobetheatre.com")
(def max-pages 12)

(def ^:private fields "id,link,title,excerpt,content,acf,class_list")

(defn- ymd [s]
  (when (and s (re-matches #"\d{8}" s))
    (str (LocalDate/parse s DateTimeFormatter/BASIC_ISO_DATE))))

(defn- ->event [{:keys [id link title excerpt content acf class_list]}]
  (let [start (ymd (:event_start_date acf))
        end (ymd (:event_end_date acf))
        genres (->> class_list
                    (keep #(second (re-matches #"shows_genre-(.*)" %)))
                    (map #(str/capitalize (str/replace % "-" " "))))
        price-text (u/clean (:price_display acf))]
    (when (and start (not (:archived_show acf)))
      (merge
       {:source-id (str id)
        :venue "The Wardrobe Theatre"
        :title (u/text (u/parse-html (:rendered title) base))
        :subtitle (u/html->text (:rendered excerpt))
        :description (u/html->text (:rendered content))
        :start-date start
        :end-date (when (and end (not= end start)) end)
        :start-time (u/parse-time (:show_time acf))
        :date-text (u/clean (:date_display acf))
        :price-text price-text
        :url link
        :booking-url (if (and (:external_booking acf) (u/clean (:external_booking_link acf)))
                       (:external_booking_link acf)
                       link)
        :category (first genres)
        :tags (vec (concat genres (keep u/clean [(:suitable_for acf) (:duration_minutes acf)])))
        :sold-out? (boolean (:external_tickets_sold_out acf))}
       (u/parse-prices price-text)))))

(defn events [{:keys [from to]}]
  ;; posts come newest-published first; stop at the first page with nothing upcoming
  (loop [page 1 acc []]
    (let [batch (http/fetch-json (http/url (str base "/wp-json/wp/v2/shows")
                                           [[:per_page 100] [:page page] [:_fields fields]]))
          evs (keep ->event batch)
          upcoming (filter #(u/in-window? % from to) evs)
          acc (into acc upcoming)]
      (u/log (format "  [wardrobe] page %d: %d shows, %d upcoming" page (count batch) (count upcoming)))
      (if (or (< (count batch) 100) (empty? upcoming) (>= page max-pages))
        acc
        (recur (inc page) acc)))))
