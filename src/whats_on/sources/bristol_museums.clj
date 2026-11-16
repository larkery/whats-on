(ns whats-on.sources.bristol-museums
  "Bristol Museums: M Shed, Bristol Museum & Art Gallery, Blaise Museum, the
  Georgian House, Red Lodge etc., all on one paged listing. Venue and event type
  are in each card's CSS classes. Event pages add times and prices."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.bristolmuseums.org.uk")
(def max-pages 10)

(def ^:private venue-names
  {"m-shed" "M Shed"
   "bristol-museum-and-art-gallery" "Bristol Museum & Art Gallery"
   "bristol-archives" "Bristol Archives"
   "blaise-castle-house-museum" "Blaise Castle House Museum"
   "georgian-house-museum" "The Georgian House Museum"
   "red-lodge-museum" "The Red Lodge Museum"
   "kings-weston-roman-villa" "Kings Weston Roman Villa"})

(defn- class-values [c prefix]
  (keep #(when (str/starts-with? % prefix) (subs % (count prefix))) (.classNames c)))

(defn- ->event [c]
  (let [url (u/abs-href-at c "h1 a")
        date-text (u/text-at c ".archive__post__date")
        [start end] (u/parse-dates date-text)
        venue-slug (first (class-values c "venues-"))
        types (->> (u/sel c ".event-type") (keep u/text) vec)]
    (when (and url start)
      {:source-id (u/slug-id url)
       :venue (venue-names venue-slug (some-> venue-slug (str/replace "-" " ") str/capitalize))
       :title (u/text-at c "h1")
       :subtitle (u/text-at c ".is-typeset")
       :start-date (str start)
       :end-date (when (and end (not= end start)) (str end))
       :date-text date-text
       :url url
       :booking-url url
       :category (first types)
       :tags types})))

(defn- with-details [{:keys [url] :as e}]
  (try
    (let [doc (u/parse-html (http/fetch url {:max-age-h (* 6 24)}) url)
          body (u/text doc)
          price-text (some->> body
                              (re-find #"(?i)(?:price|cost|tickets?)\s*:?\s*((?:£\d[\d.]*|free)[^.]{0,60})")
                              second
                              ;; stop at the booking button that follows
                              (#(str/replace % #"(?i)\s*(buy|book) now.*$" ""))
                              u/clean)]
      (merge e
             ;; the event body; .alert-message blocks above it are site-wide banners
             {:description (or (u/paragraphs doc ".as-wysiwyg p")
                               (:subtitle e)
                               (u/meta-description doc))
              ;; "Sessions begin at 10:30am", "Start time: 7pm", "Starts 6.30pm"
              :start-time (some->> body
                                   (re-find #"(?i)\b(?:time|starts?|begins?)(?:\s+at)?\s*:?\s*(\d{1,2}(?:[:.]\d{2})?\s*(?:am|pm))")
                                   second u/parse-time)
              :price-text price-text
              :image (u/og-image doc)}
             (u/parse-prices price-text)))
    (catch Exception ex
      (u/log "  [bristol-museums] details failed" url (ex-message ex))
      e)))

(defn- listing-page [n]
  (let [url (str base "/whats-on/" (when (> n 1) (str "page/" n "/")))
        doc (u/parse-html (http/fetch url) base)]
    {:events (keep ->event (u/sel doc "article.type-bm-events"))
     :more? (boolean (u/sel1 doc (str "a[href*=/whats-on/page/" (inc n) "/]")))}))

(defn events [{:keys [from to]}]
  (loop [n 1 acc []]
    (let [{:keys [events more?]} (listing-page n)
          acc (into acc events)]
      (if (and more? (< n max-pages))
        (recur (inc n) acc)
        (->> acc
             (filter #(u/in-window? % from to))
             (map with-details))))))
