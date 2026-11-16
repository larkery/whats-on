(ns whats-on.sources.hen-and-chicken
  "The Hen & Chicken, Southville, via The Events Calendar API. Mostly comedy, which
  is left out; talks ('Talk: …') and music are kept."
  (:require [clojure.string :as str]
            [whats-on.sources.tribe :as tribe]))

(def ^:private unwanted #"(?i)comedy|improv|antenatal")

(defn- keep? [e]
  (let [prefix (second (re-find #"^([^:]{2,20}):" (str (:title e))))]
    (or (some->> prefix (re-find #"(?i)talk"))
        (contains? (tribe/category-names e) "talks")
        (not (or (some->> prefix (re-find unwanted))
                 (some #(re-find unwanted %) (tribe/category-names e)))))))

(defn events [opts]
  (->> (tribe/events "https://www.henandchicken.com"
                     (assoc opts :venue "The Hen & Chicken" :keep? keep?))
       (map #(cond-> %
               (str/starts-with? (str (:title %)) "Talk:") (assoc :category "Talk")))))
