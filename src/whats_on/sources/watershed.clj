(ns whats-on.sources.watershed
  "Watershed cinema. The listing has one card per film; each film's page lists
  its screenings (time + box office link), so we read those for showtimes."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.watershed.co.uk")

(defn- card [c]
  {:url (u/abs-href-at c ".whatson-card__title a")
   :title (u/text-at c ".whatson-card__title h3")
   :rating (some-> (u/text-at c ".whatson-card__rating") (str/replace #"^Rated\s*" ""))
   :programme (u/text-at c ".whatson-card__programme")
   :copy (u/clean (some-> (u/sel1 c ".whatson-card__copy") .ownText))
   :image (u/attr (u/sel1 c ".whatson-card__image img") "src")})

(defn- screenings [doc]
  (for [b (u/sel doc "[data-component-id=watershed:event-button]")
        :let [dt (u/attr (u/sel1 b "time") "datetime")
              [date time] (u/iso->local dt)
              href (or (u/attr b "href") (u/abs-href b))
              status (u/text-at b ".event-button__status")
              tags (->> (u/sel b ".event-button__suffix .pill, .event-button__suffix dt, .event-button__tag")
                        (keep u/text) vec)]
        :when date]
    {:start-date date
     :start-time time
     :booking-url href
     :status status
     :screening-id (or (second (re-find #"/events/(\d+)" (str href))) (str date "T" time))
     :tags tags}))

(defn- film-events [{:keys [url title rating programme copy image]}]
  (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
        ld (first (u/jsonld-events doc))
        desc (or (some-> (:description ld) (str/replace #"\s*Book now\.?\s*$" "") u/clean)
                 copy)
        film-id (second (re-find #"/whatson/(\d+)" url))]
    (for [s (screenings doc)]
      {:source-id (str film-id "-" (:screening-id s))
       :venue "Watershed"
       :title title
       :description desc
       :start-date (:start-date s)
       :start-time (:start-time s)
       :url url
       :booking-url (:booking-url s)
       :category (if (and ld (get-in ld [:workFeatured (keyword "@type")])) "Film" (or programme "Film"))
       :tags (vec (distinct (concat (when rating [(str "Rated " rating)])
                                    (when programme [programme])
                                    (:tags s))))
       :image image
       :sold-out? (boolean (some->> (:status s) (re-find #"(?i)sold out")))})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/whatson")) base)
        cards (keep #(let [c (card %)] (when (:url c) c)) (u/sel doc ".whatson-card"))]
    (u/log (format "  [watershed] %d films" (count cards)))
    (->> cards
         (mapcat (fn [c]
                   (try (film-events c)
                        (catch Exception e
                          (u/log "  [watershed] failed" (:url c) (ex-message e))
                          []))))
         (filter #(u/in-window? % from to)))))
