(ns whats-on.sources.storysmith
  "Storysmith Books, North Street. Events are WooCommerce products: the category
  page has the date ('Thursday 3rd December 2026, 6:30 pm'), price and stock;
  the WooCommerce Store API adds the description."
  (:require [clojure.string :as str]
            [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://storysmithbooks.com")

(defn- ->event [c]
  (let [url (u/abs-href-at c ".wp-block-post-title a")
        when-text (u/text-at c ".wp-block-storysmith-blocks-event-date")
        date (first (u/parse-dates when-text))
        ;; "£8.00 – £25.00Price range: £8.00 through £25.00" (the second half is screen-reader text)
        price-text (some-> (u/text-at c ".wc-block-components-product-price")
                           (str/replace #"Price range:.*$" "")
                           u/clean)]
    (when (and url date)
      (merge
       {:source-id (u/slug-id url)
        :venue "Storysmith Books"
        :title (u/text-at c ".wp-block-post-title")
        :start-date (str date)
        :start-time (u/parse-time when-text)
        :price-text price-text
        :url url
        :booking-url url
        :category "Books & talks"
        :sold-out? (.hasClass c "outofstock")}
       (u/parse-prices price-text)))))

(defn- descriptions []
  (try
    (->> (http/fetch-json (http/url (str base "/wp-json/wc/store/v1/products")
                                    [[:category "upcoming-events"] [:per_page 50]]))
         (map (fn [p] [(u/slug-id (:permalink p))
                       (some-> (:short_description p)
                               u/html->text
                               ;; drop the address line every event starts with
                               (str/replace #"^Storysmith, 236 North Street[^\n]*\n*" "")
                               u/clean-paragraphs)]))
         (into {}))
    (catch Exception e
      (u/log "  [storysmith] descriptions failed" (ex-message e))
      {})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/product-category/upcoming-events/")) base)
        evs (->> (u/sel doc "li.wc-block-product, li.product")
                 (keep ->event)
                 (filter #(u/in-window? % from to)))
        descs (when (seq evs) (descriptions))]
    (map #(assoc % :description (get descs (:source-id %))) evs)))
