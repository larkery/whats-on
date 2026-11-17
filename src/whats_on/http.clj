(ns whats-on.http
  "Polite HTTP fetching:
  - one request at a time per host, spaced by a delay (or the site's Crawl-delay if longer)
  - an on-disk cache, so recently fetched pages are not re-requested, plus
    conditional requests (ETag / Last-Modified) when the cache is stale
  - robots.txt is honoured unless a source explicitly opts out
  - an honest User-Agent, and backoff on 429 / 5xx."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [whats-on.robots :as robots]
            [whats-on.util :as u])
  (:import [java.net URI URLEncoder]
           [java.security MessageDigest]))

(def config
  (atom {:delay-ms 4000
         :contact nil
         :cache-dir (str (fs/path (or (System/getenv "XDG_CACHE_HOME")
                                      (str (fs/home) "/.cache"))
                                  "whats-on" "http"))}))

(def stats (atom {}))

(defn- user-agent []
  (str "whats-on/0.1 (personal events reader"
       (when-let [c (:contact @config)] (str "; " c))
       ")"))

(def ^:private log u/log)

;;; --- cache --------------------------------------------------------------

(defn- sha1 [s]
  (let [d (.digest (MessageDigest/getInstance "SHA-1") (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) d))))

(defn- cache-paths [url]
  (let [h (sha1 url)
        dir (fs/path (:cache-dir @config) (subs h 0 2))]
    {:dir dir
     :meta (fs/path dir (str h ".edn"))
     :body (fs/path dir (str h ".body"))}))

(defn- read-cache [url]
  (let [{:keys [meta body]} (cache-paths url)]
    (when (and (fs/exists? meta) (fs/exists? body))
      (try (assoc (edn/read-string (slurp (str meta))) :body (slurp (str body)))
           (catch Exception _ nil)))))

(defn- write-cache! [url {:keys [body] :as entry}]
  (let [{:keys [dir meta] :as p} (cache-paths url)]
    (fs/create-dirs dir)
    (spit (str (:body p)) body)
    (spit (str meta) (pr-str (dissoc entry :body)))))

;;; --- per-host pacing ----------------------------------------------------

(defonce ^:private host-locks (atom {}))
(defonce ^:private next-slot (atom {}))

(defn- host-lock [host]
  (get (swap! host-locks #(if (% host) % (assoc % host (Object.)))) host))

(defn- wait-turn!
  "Block until it's this host's turn, then reserve the next slot."
  [host delay-ms]
  (locking (host-lock host)
    (let [wait (- (get @next-slot host 0) (System/currentTimeMillis))]
      (when (pos? wait) (Thread/sleep (long wait))))
    ;; +/- 20% jitter so we don't tick like a metronome
    (let [jittered (long (* delay-ms (+ 0.8 (rand 0.4))))]
      (swap! next-slot assoc host (+ (System/currentTimeMillis) jittered)))))

;;; --- raw request --------------------------------------------------------

(declare robots-for)

(defn- host-delay [host]
  (let [crawl-delay (some-> (robots-for host) :crawl-delay (* 1000) long)]
    (max (:delay-ms @config) (or crawl-delay 0))))

(defn- request!
  "One paced GET with retries on 429/5xx. Returns the http-client response."
  [url headers]
  (let [host (.getHost (URI. url))]
    (loop [attempt 1]
      (wait-turn! host (if (= url (str "https://" host "/robots.txt"))
                         (:delay-ms @config)
                         (host-delay host)))
      (swap! stats update-in [host :requests] (fnil inc 0))
      (let [resp (try (http/get url {:headers (merge {"User-Agent" (user-agent)
                                                      "Accept-Language" "en-GB,en;q=0.8"}
                                                     headers)
                                     :throw false
                                     :timeout 30000})
                      (catch Exception e {:status -1 :error e}))
            status (:status resp)]
        (if (and (or (= status 429) (>= status 500) (= status -1)) (< attempt 3))
          (let [retry-after (some-> (get-in resp [:headers "retry-after"]) parse-long)
                backoff (* 1000 (or retry-after (* 15 attempt)))]
            (log (format "  %s -> %s, retrying in %ds" url status (quot backoff 1000)))
            (Thread/sleep (long backoff))
            (recur (inc attempt)))
          resp)))))

;;; --- robots.txt ---------------------------------------------------------

(defonce ^:private robots-cache (atom {}))

(defn reset-run!
  "Start a run: zero the request counts, and re-read robots.txt (from the disk
  cache if under a day old) rather than keeping it for as long as the app runs."
  []
  (reset! stats {})
  (reset! robots-cache {}))

(defn- fetch-robots [host]
  (let [url (str "https://" host "/robots.txt")
        cached (read-cache url)
        fresh? (and cached (< (- (System/currentTimeMillis) (:fetched-at cached)) (* 24 3600 1000)))
        txt (if fresh?
              (:body cached)
              (let [{:keys [status body]} (request! url {})]
                (let [txt (if (= 200 status) body "")]
                  (write-cache! url {:url url :status status :fetched-at (System/currentTimeMillis) :body txt})
                  txt)))]
    (robots/rules-for (robots/parse txt) "whats-on")))

(defn- robots-for [host]
  (or (get @robots-cache host)
      (let [r (fetch-robots host)]
        (swap! robots-cache assoc host r)
        r)))

;;; --- public API ---------------------------------------------------------

(defn url
  "Build a URL from a base and ordered [k v] pairs (or a map)."
  [base params]
  (if (empty? params)
    base
    (str base (if (str/includes? base "?") "&" "?")
         (str/join "&" (for [[k v] params]
                         (str (URLEncoder/encode (name k) "UTF-8") "="
                              (URLEncoder/encode (str v) "UTF-8")))))))

(defn fetch
  "GET `url`, returning the body as a string.

  Options:
    :max-age-h      serve from cache without any request if younger than this (default 12)
    :ignore-robots? skip the robots.txt check (only for sources where that's been decided)
    :headers        extra request headers

  Throws ex-info on a non-2xx response or a robots.txt disallow."
  ([u] (fetch u {}))
  ([u {:keys [max-age-h ignore-robots? headers] :or {max-age-h 12}}]
   (let [uri (URI. u)
         host (.getHost uri)
         path (str (.getRawPath uri) (when-let [q (.getRawQuery uri)] (str "?" q)))
         cached (read-cache u)
         age-ms (when cached (- (System/currentTimeMillis) (:fetched-at cached)))]
     (cond
       (and cached (< age-ms (* max-age-h 3600 1000)))
       (do (swap! stats update-in [host :cached] (fnil inc 0))
           (:body cached))

       (and (not ignore-robots?) (not (robots/allowed? (robots-for host) path)))
       (throw (ex-info (str "robots.txt disallows " u) {:url u :robots true}))

       :else
       (let [conditional (cond-> {}
                           (:etag cached) (assoc "If-None-Match" (:etag cached))
                           (:last-modified cached) (assoc "If-Modified-Since" (:last-modified cached)))
             {:keys [status body] :as resp} (request! u (merge headers conditional))]
         (cond
           (and (= 304 status) cached)
           (do (write-cache! u (assoc cached :fetched-at (System/currentTimeMillis)))
               (swap! stats update-in [host :not-modified] (fnil inc 0))
               (:body cached))

           (<= 200 status 299)
           (do (write-cache! u {:url u
                                :status status
                                :etag (get-in resp [:headers "etag"])
                                :last-modified (get-in resp [:headers "last-modified"])
                                :fetched-at (System/currentTimeMillis)
                                :body body})
               body)

           :else
           (throw (ex-info (str "HTTP " status " for " u) {:url u :status status}))))))))

(defn fetch-json [u & [opts]]
  (json/parse-string (fetch u (merge {:headers {"Accept" "application/json"}} opts)) true))
