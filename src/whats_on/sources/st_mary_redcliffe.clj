(ns whats-on.sources.st-mary-redcliffe
  "St Mary Redcliffe. Its own site renders events client-side, but it sells
  through Eventbrite collections. NB as of Nov 2026 these collections haven't
  been kept up to date, so this may find nothing until new events are added."
  (:require [whats-on.sources.eventbrite :as eb]))

(def collections
  ["/cc/live-events-1931509"
   "/cc/redcliffe-talks-3108509"
   "/cc/free-lunchtime-recitals-3094029"])

(defn events [opts]
  (eb/events collections (assoc opts :venue "St Mary Redcliffe")))
