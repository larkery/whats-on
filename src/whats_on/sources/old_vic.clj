(ns whats-on.sources.old-vic
  "Bristol Old Vic. Paged listing of productions; each production page has
  JSON-LD with one Event per performance (time, price, booking link)."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://bristololdvic.org.uk")
(def max-pages 12)

(defn- listing-page [n]
  (let [url (str base "/whats-on" (when (> n 1) (str "/page-" n)))
        doc (u/parse-html (http/fetch url) base)]
    (for [c (u/sel doc "a.c-card--event")]
      {:url (u/abs-href c)
       :title (u/text-at c ".c-card__title")
       :first-date (some-> (u/attr (u/sel1 c "time") "datetime") u/iso->local first)
       :image (u/attr (u/sel1 c "img") "src")})))

(defn- productions [to]
  (loop [n 1 acc []]
    (let [cards (listing-page n)]
      (if (or (empty? cards)
              (>= n max-pages)
              ;; listing is date-ordered: stop once a whole page starts after the window
              (every? #(pos? (compare (str (:first-date %)) (str to))) cards))
        (into acc cards)
        (recur (inc n) (into acc cards))))))

(defn- performances [{:keys [url title image]}]
  (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
        desc (u/meta-description doc)
        crumbs (->> (u/sel doc ".c-breadcrumbs a, .c-tag, .c-event-meta__genre") (keep u/text) distinct vec)]
    (for [ev (u/jsonld-events doc)
          :let [[date time] (u/iso->local (:startDate ev))
                offer (let [o (:offers ev)] (if (sequential? o) (first o) o))
                price-text (some-> (:price offer) str u/clean)]]
      (merge
       {:source-id (str (u/slug-id url) "@" date "T" time)
        :venue "Bristol Old Vic"
        :title (or title (u/clean (:name ev)))
        :description desc
        :start-date date
        :start-time time
        :price-text price-text
        :url url
        :booking-url (or (:url offer) url)
        :tags crumbs
        :image image
        :sold-out? (boolean (some->> (:availability offer) str (re-find #"(?i)soldout")))}
       (u/parse-prices price-text)))))

(defn events [{:keys [from to]}]
  (let [prods (->> (productions to) (filter :url) (remove #(pos? (compare (str (:first-date %)) (str to)))))]
    (u/log (format "  [old-vic] %d productions" (count prods)))
    (->> prods
         (mapcat #(try (performances %)
                       (catch Exception e
                         (u/log "  [old-vic] failed" (:url %) (ex-message e))
                         [])))
         (filter #(u/in-window? % from to)))))
