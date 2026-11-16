(ns whats-on.sources.st-georges
  "St George's Bristol. Paged listing with one card per performance (date only);
  each event page has JSON-LD with the start time and price range."
  (:require [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.stgeorgesbristol.co.uk")
(def max-pages 15)

(defn- listing-page [n]
  (let [url (str base "/whats-on/" (when (> n 1) (str "page/" n "/")))
        doc (u/parse-html (http/fetch url) base)]
    (for [c (u/sel doc ".c-col-card--event")
          :let [date (first (u/parse-dates (u/text-at c ".c-col-card__date")))]
          :when date]
      {:url (u/abs-href-at c "a.c-col-card__link")
       :booking-url (u/abs-href-at c ".c-col-card__book a")
       :title (u/text-at c ".c-col-card__title")
       :subtitle (u/text-at c ".c-col-card__subtitle")
       :category (u/text-at c ".c-col-card__taxonomy")
       :start-date (str date)
       :image (u/attr (u/sel1 c "img") "src")})))

(defn- cards [to]
  (loop [n 1 acc []]
    (let [page (listing-page n)
          acc (into acc page)]
      (if (or (empty? page)
              (>= n max-pages)
              (pos? (compare (:start-date (last page)) (str to))))
        acc
        (recur (inc n) acc)))))

(defn- with-details [{:keys [url booking-url start-date] :as c}]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
          evs (u/jsonld-events doc)
          ;; pages can hold several performances; pick the one this card is for
          ev (or (first (filter #(= start-date (first (u/iso->local (:startDate %)))) evs))
                 (first evs))
          [_ time] (u/iso->local (:startDate ev))
          offer (let [o (:offers ev)] (if (sequential? o) (first o) o))
          lo (some-> (:lowPrice offer) u/parse-prices :price-min)
          hi (some-> (:highPrice offer) u/parse-prices :price-max)
          from-text (u/text-at doc ".c-event-header__price, .c-book-panel__price, [class*=price]")]
      (merge c
             {:start-time time
              :description (or (u/clean (:description ev))
                               (u/paragraphs doc ".event__important-information p")
                               (u/meta-description doc))
              :price-min lo
              :price-max (or hi lo)
              :price-text from-text
              :booking-url (or booking-url (:url offer))
              :sold-out? (boolean (some->> (:availability offer) str (re-find #"(?i)soldout")))}))
    (catch Exception e
      (u/log "  [st-georges] details failed" url (ex-message e))
      c)))

(defn events [{:keys [from to]}]
  (->> (cards to)
       (filter #(u/in-window? % from to))
       (map with-details)
       (map #(merge {:source-id (str (u/slug-id (:url %)) "@" (:start-date %))
                     :venue "St George's Bristol"
                     :tags (some-> (:category %) vector)}
                    %))))
