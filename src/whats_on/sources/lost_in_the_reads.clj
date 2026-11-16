(ns whats-on.sources.lost-in-the-reads
  "Lost in the Reads, a bookshop with author events, sold through TicketTailor."
  (:require [whats-on.sources.tickettailor :as tt]))

(defn events [opts]
  (tt/events "lostinthereads" (assoc opts :venue "Lost in the Reads" :category "Books & talks")))
