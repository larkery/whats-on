(ns whats-on.app.main
  "Local web app: `clojure -M:app [port]`, then open http://localhost:8787"
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as http]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [whats-on.app.refresh :as refresh]
            [whats-on.app.store :as store]
            [whats-on.app.web :as web])
  (:gen-class)
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(defn- html [body] {:status 200 :headers {"Content-Type" "text/html; charset=utf-8"} :body body})
(defn- redirect [to] {:status 303 :headers {"Location" to}})

(defn- param-list
  "A form field that may be repeated, as a seq."
  [params k]
  (let [v (get params k)] (cond (nil? v) nil (string? v) [v] :else v)))

(declare wake-embedder!)

(defn- routes [ds cfg]
  (fn [{:keys [request-method uri params]}]
    (let [[_ id] (re-matches #"/(?:statements|history)/(\d+)/delete" uri)]
      (cond
        (= uri "/") (redirect "/plan")
        (= [request-method uri] [:get "/plan"]) (html (web/plan-page ds params))
        (= [request-method uri] [:get "/find"]) (html (web/find-page ds params))
        (= [request-method uri] [:get "/rate"]) (html (web/rate-page ds params))
        (= [request-method uri] [:get "/taste"]) (html (web/taste-page ds params))
        (= [request-method uri] [:get "/refresh"]) (html (web/refresh-page ds cfg))
        (= [request-method uri] [:get "/refresh/progress"]) (html (web/refresh-panel ds cfg))
        (= [request-method uri] [:post "/refresh"])
        (do (refresh/start! ds cfg {:only (map keyword (param-list params "only"))
                                    :on-done wake-embedder!})
            (redirect "/refresh"))
        (= [request-method uri] [:get "/status"]) (html (web/status-fragment))
        (= [request-method uri] [:post "/feedback"]) (html (or (web/feedback! ds params) ""))
        (= [request-method uri] [:post "/feedback/clear"])
        (do (store/set-feedback! ds {:key (get params "key")} "none") (redirect "/taste"))
        (= [request-method uri] [:post "/hide"]) (html (or (web/hide! ds params) ""))
        (= [request-method uri] [:post "/hidden/clear"])
        (do (store/set-hidden! ds {:key (get params "key")} false) (redirect "/taste"))
        (= [request-method uri] [:post "/statements"])
        (do (when-not (str/blank? (get params "text"))
              (store/add-statement! ds (str/trim (get params "text")) (parse-double (get params "weight" "1"))))
            (redirect "/taste"))
        (= [request-method uri] [:post "/history"])
        (do (web/add-history-lines! ds (get params "lines")) (redirect "/taste"))
        (and id (= request-method :post) (str/starts-with? uri "/statements/"))
        (do (store/delete-statement! ds (parse-long id)) (redirect "/taste"))
        (and id (= request-method :post) (str/starts-with? uri "/history/"))
        (do (store/delete-history! ds (parse-long id)) (redirect "/taste"))
        :else {:status 404 :body "Not found"}))))

(defn- wrap-errors [handler]
  (fn [req]
    (try (handler req)
         (catch Exception e
           (.printStackTrace e)
           {:status 500 :headers {"Content-Type" "text/plain"} :body (str "Error: " (ex-message e))}))))

(defn- embed-pending!
  "Embed every upcoming event we haven't seen yet. Cheap when nothing's new."
  [ds]
  (let [todo (->> (store/items ds) (remove #(@store/vectors (:hash %))) (map :text) distinct vec)]
    (when (seq todo)
      (reset! web/status {:embedded 0 :total (count todo)})
      (println "Embedding" (count todo) "event descriptions…")
      (doseq [t todo]
        (store/vector-for ds t)
        (swap! web/status update :embedded inc))
      (println "Embedding done."))))

(defonce ^:private wake (LinkedBlockingQueue.))

(defn- wake-embedder!
  "Embed new events now, e.g. after a refresh, rather than at the next tick."
  []
  (.offer wake :now))

(defn- start-embedder! [ds]
  (future
    (loop []
      (try (embed-pending! ds)
           (catch Throwable e (println "Embedding failed:" (ex-message e))))
      ;; also pick up new events from `clojure -M:scrape` without a restart
      (.poll wake 10 TimeUnit/MINUTES)
      (recur))))

(defn- config []
  (merge {:db "whats-on.db" :port 8787}
         (when (.exists (io/file "config.edn")) (edn/read-string (slurp "config.edn")))))

(defn -main [& [port]]
  (let [{:keys [db] :as cfg} (config)
        port (or (some-> port parse-long) (:port cfg))
        ds (store/datasource db)]
    (store/load-vectors! ds)
    (start-embedder! ds)
    (http/run-server (-> (routes ds cfg)
                         wrap-params
                         (wrap-resource "public")
                         wrap-content-type
                         wrap-errors)
                     {:port port :ip "127.0.0.1"})
    (println (str "What's on: http://localhost:" port))))
