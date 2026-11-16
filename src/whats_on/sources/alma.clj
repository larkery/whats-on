(ns whats-on.sources.alma
  "Alma Tavern & Theatre. The venue's own site is a Wix page with no listings;
  the theatre sells through TicketTailor, which lists every event."
  (:require [whats-on.sources.tickettailor :as tt]))

(defn events [opts]
  (tt/events "almatheatrecompany" (assoc opts :venue "Alma Tavern & Theatre" :category "Theatre")))
