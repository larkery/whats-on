(ns whats-on.db
  "Reads for the bb tasks. The app owns the schema and the writes; see
  whats-on.app.store and whats-on.app.refresh."
  (:require [pod.babashka.go-sqlite3 :as sqlite]))

(defn upcoming [db {:keys [to q category]}]
  (sqlite/query db (into [(str "select * from upcoming where start_date <= ?"
                               (when q " and (title like ? or subtitle like ? or description like ? or venue like ? or tags like ?)")
                               (when category " and (category like ? or tags like ?)")
                               " order by start_date, coalesce(start_time, '99'), venue, title")
                          (str to)]
                         (concat (when q (repeat 5 (str "%" q "%")))
                                 (when category (repeat 2 (str "%" category "%")))))))
