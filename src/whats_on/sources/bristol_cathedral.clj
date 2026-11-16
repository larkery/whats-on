(ns whats-on.sources.bristol-cathedral
  "Bristol Cathedral, via The Events Calendar API. The calendar also holds every
  daily service, so those categories are dropped."
  (:require [whats-on.sources.tribe :as tribe]))

(def ^:private skip #{"services" "special services"})

(defn events [opts]
  (tribe/events "https://bristol-cathedral.co.uk"
                (assoc opts
                       :venue "Bristol Cathedral"
                       :keep? #(let [cats (tribe/category-names %)]
                                 (and (seq cats) (not-every? skip cats))))))
