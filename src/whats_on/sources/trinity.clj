(ns whats-on.sources.trinity
  "Trinity Centre. One listing page; cards carry title, a one-line blurb and a
  dd/mm/yyyy hh:mm AM/PM start. The page also has some stale past entries,
  which the date window filters out."
  (:require [whats-on.http :as http]
            [whats-on.util :as u]))

(def base "https://www.trinitybristol.org.uk")

(defn- ->event [c]
  (let [url (u/abs-href-at c "a")
        when-text (u/text-at c ".caption__overlay__time")
        date (u/parse-numeric-date when-text)]
    (when (and url date)
      {:source-id (u/slug-id url)
       :venue "Trinity Centre"
       :title (u/text-at c ".caption__overlay__title")
       :subtitle (u/text-at c ".caption__overlay__content")
       :start-date (str date)
       :start-time (u/parse-time when-text)
       :url url
       :booking-url url
       :image (u/abs-url (u/sel1 c "img") "src")})))

(defn events [{:keys [from to]}]
  (let [doc (u/parse-html (http/fetch (str base "/whats-on")) base)]
    (->> (u/sel doc ".fragletItem-Event")
         (keep ->event)
         (filter #(u/in-window? % from to)))))
