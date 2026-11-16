(ns whats-on.sources.headfirst
  "Headfirst Bristol — the big listings aggregator, including lots of small venues.

  NB robots.txt disallows /api/v2. Its What's On page is rendered in the browser
  from that same endpoint, and this tool reads it the way the browser would, on
  the user's behalf (an accessibility-motivated plain-text reader), at ~20
  requests a week. So this source opts out of the robots check. Revisit if
  Headfirst ever publish a feed or object."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.headfirstbristol.co.uk")
(def page-size 200)

(def categories {"gig" "Music" "club" "Club night" "other" "Arts & other"})

(defn- page [from to offset]
  (http/fetch-json
   (http/url (str base "/api/v2/events")
             [[:bundle "publicWebsite"]
              ["filter[0][field]" "date"] ["filter[0][operator]" ">="] ["filter[0][value]" from]
              ["filter[1][field]" "date"] ["filter[1][operator]" "<="] ["filter[1][value]" to]
              ;; order by id so offset paging is stable
              [:orderBy "event_id"] [:orderDirection "asc"]
              [:limit page-size] [:offset offset]])
   {:ignore-robots? true}))

(defn- prices [e]
  (let [fvs (->> (:productListings e)
                 (filter :isPublished)
                 (remove :isSecret)
                 (remove :hasReachedMaxTimesToSell)
                 (keep :faceValue)
                 (map #(/ % 100.0)))]
    (when (seq fvs) {:price-min (apply min fvs) :price-max (apply max fvs)})))

(defn- ->event [e]
  (let [url (str base "/#e" (:event_id e))]
    (merge
     {:source-id (str (:event_id e))
      :venue (u/clean (:venue_name e))
      :title (u/clean (:event_name e))
      :subtitle (u/clean (:desc_short e))
      :description (u/clean-paragraphs (:desc_long e))
      :start-date (:date e)
      :start-time (or (:start_time e) (:doors_time e))
      :end-time (:end_time e)
      :price-text (when-not (= "See event details" (:pricing_detail e)) (u/clean (:pricing_detail e)))
      :url url
      :booking-url url
      :category (categories (:type e) (:type e))
      :tags (->> (concat (map :subGenreName (:subGenres e))
                         [(:entry_requirements e)])
                 (keep u/clean) distinct vec)
      :image (some->> (:images e) first :src (format "%s/images/events/%s_s5.jpg" base))
      :sold-out? (boolean (:isSoldOut e))}
     (prices e))))

(defn events [{:keys [from to]}]
  (loop [offset 0 acc []]
    (let [batch (:events (page (str from) (str to) offset))
          acc (into acc batch)]
      (u/log (format "  [headfirst] offset %d: %d events" offset (count batch)))
      (if (= page-size (count batch))
        (recur (+ offset page-size) acc)
        (->> acc
             (remove :isCancelled)
             (map ->event))))))
