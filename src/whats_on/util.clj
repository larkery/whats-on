(ns whats-on.util
  "Shared parsing helpers: HTML, text, dates, prices, JSON-LD."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.time LocalDate LocalDateTime OffsetDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [org.jsoup Jsoup]))

(def london (ZoneId/of "Europe/London"))

(defn today [] (LocalDate/now london))

(def ^:dynamic *sink*
  "If bound, a fn also given each logged line (the app's Refresh page shows them)."
  nil)

(defn log [& xs]
  (binding [*out* *err*] (apply println xs))
  (when *sink* (*sink* (str/join " " xs))))

;;; --- HTML ---------------------------------------------------------------

(defn parse-html [html base-url] (Jsoup/parse html base-url))

(defn sel [el css] (if el (vec (.select el css)) []))
(defn sel1 [el css] (when el (.selectFirst el css)))

(defn clean
  "Collapse whitespace; nil if blank."
  [s]
  (when s
    (let [s (-> (str s) (str/replace #"[ \s]+" " ") str/trim)]
      (when-not (str/blank? s) s))))

(defn clean-paragraphs
  "Like clean, but keeps paragraph breaks. Strips markdown bold/italic markers."
  [s]
  (some-> s
          (str/replace #"\r" "")
          (str/replace #"\*\*|__" "")
          (str/replace #"[ \t ]+" " ")
          (str/replace #" *\n *" "\n")
          (str/replace #"\n{3,}" "\n\n")
          str/trim
          not-empty))

(defn text [el] (when el (clean (.text el))))
(defn text-at [el css] (text (sel1 el css)))
(defn attr [el a] (when el (clean (.attr el a))))
(defn abs-url [el a] (when el (clean (.absUrl el a))))
(defn abs-href [el] (abs-url el "href"))
(defn abs-href-at [el css] (abs-href (sel1 el css)))

(defn html->text
  "Turn an HTML fragment into readable plain text, keeping paragraph breaks."
  [html]
  (when html
    (let [marked (-> html
                     (str/replace #"(?i)<br\s*/?>" "\n")
                     (str/replace #"(?i)</(p|div|li|h[1-6]|tr)>" "\n\n"))
          t (.wholeText (.body (Jsoup/parse marked)))]
      (some-> t
              (str/replace #"[ \t ]+" " ")
              (str/replace #" *\n *" "\n")
              (str/replace #"\n{3,}" "\n\n")
              str/trim
              not-empty))))

(defn decode-entities
  "Some sites double-encode entities (e.g. &amp;#039; in meta tags)."
  [s]
  (some-> s (Jsoup/parse) .text clean))

(defn meta-description [doc]
  (decode-entities
   (or (attr (sel1 doc "meta[property=og:description]") "content")
       (attr (sel1 doc "meta[name=description]") "content"))))

(defn paragraphs
  "Readable text of the substantial <p>s matching css, joined as paragraphs."
  [doc css & {:keys [min-len] :or {min-len 40}}]
  (some->> (sel doc css)
           (keep text)
           (filter #(>= (count %) min-len))
           distinct
           seq
           (str/join "\n\n")))

(defn og-image [doc]
  (attr (sel1 doc "meta[property=og:image]") "content"))

;;; --- JSON-LD ------------------------------------------------------------

(defn- flatten-ld [x]
  (cond
    (sequential? x) (mapcat flatten-ld x)
    (map? x) (if-let [g (get x (keyword "@graph"))] (flatten-ld g) [x])
    :else []))

(defn jsonld
  "All JSON-LD objects on the page (with @graph flattened)."
  [doc]
  (->> (sel doc "script[type=application/ld+json]")
       (mapcat #(try (flatten-ld (json/parse-string (.data %) true))
                     (catch Exception _ [])))))

(defn jsonld-events [doc]
  (filter #(let [t (get % (keyword "@type"))
                 ts (if (sequential? t) t [t])]
             (some (fn [t] (and (string? t) (str/ends-with? t "Event"))) ts))
          (jsonld doc)))

;;; --- dates & times ------------------------------------------------------

(def months
  {"jan" 1 "feb" 2 "mar" 3 "apr" 4 "may" 5 "jun" 6
   "jul" 7 "aug" 8 "sep" 9 "oct" 10 "nov" 11 "dec" 12})

(defn month-num [s]
  (when (and s (>= (count s) 3))
    (months (str/lower-case (subs s 0 3)))))

(defn infer-year
  "For a day/month with no year: this year, unless that's well in the past, in which case next year."
  [m d]
  (let [t (today)
        candidate (LocalDate/of (.getYear t) (int m) (int d))]
    (if (.isBefore candidate (.minusDays t 60)) (.plusYears candidate 1) candidate)))

(def date-re
  #"(?i)\b(\d{1,2})(?:st|nd|rd|th)?\s+(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec)[a-z]*\.?(?:,?\s+(\d{4}))?")

(defn parse-dates
  "Find all day-month[-year] dates in free text like 'Tue 24 Nov - Sat 28 Nov 2026'.
  Missing years are taken from a later date in the same string, or inferred."
  [s]
  (let [found (for [[_ d m y] (re-seq date-re (or s ""))]
                {:d (parse-long d) :m (month-num m) :y (some-> y parse-long)})
        found (vec found)]
    (vec
     (for [i (range (count found))
           :let [{:keys [d m y]} (found i)
                 later-y (some :y (subvec found i))
                 later-m (:m (first (filter :y (subvec found i))))]]
       (cond
         y (LocalDate/of (int y) (int m) (int d))
         later-y (LocalDate/of (int (if (and later-m (> m later-m)) (dec later-y) later-y)) (int m) (int d))
         :else (infer-year m d))))))

(defn parse-numeric-date
  "dd/mm/yyyy"
  [s]
  (when-let [[_ d m y] (re-find #"(\d{1,2})/(\d{1,2})/(\d{4})" (or s ""))]
    (LocalDate/of (parse-long y) (parse-long m) (parse-long d))))

(defn parse-time
  "'7.30pm', '19:00', '3:00 PM', '8pm', '12:00 PM' -> \"HH:MM\"
  The space before am/pm may be a (narrow) no-break space, as Java and ICU
  date formatting now write it."
  [s]
  (when s
    (if-let [[_ h m ap] (re-find #"(?i)\b(\d{1,2})(?:[:.](\d{2}))?[\s  ]*(am|pm)\b" s)]
      (let [h (mod (parse-long h) 12)
            h (if (= "pm" (str/lower-case ap)) (+ h 12) h)]
        (format "%02d:%s" h (or m "00")))
      (when-let [[_ h m] (re-find #"\b([01]?\d|2[0-3])[:.]([0-5]\d)\b" s)]
        (format "%02d:%s" (parse-long h) m)))))

(defn iso->local
  "ISO date-time (with or without offset) -> [\"yyyy-MM-dd\" \"HH:mm\"] in UK local time."
  [s]
  (when s
    (let [ldt (cond
                (re-find #"(Z|[+-]\d{2}:?\d{2})$" s)
                (.toLocalDateTime (.atZoneSameInstant (OffsetDateTime/parse (str/replace s #"Z$" "+00:00")) london))
                (str/includes? s "T") (LocalDateTime/parse s)
                :else nil)]
      (if ldt
        [(str (.toLocalDate ldt)) (.format ldt (DateTimeFormatter/ofPattern "HH:mm"))]
        [(subs s 0 10) nil]))))

(defn in-window? [{:keys [start-date end-date]} from to]
  (let [s (str start-date) e (str (or end-date start-date))]
    (and (not (neg? (compare e (str from))))
         (not (pos? (compare s (str to)))))))

;;; --- prices -------------------------------------------------------------

(defn parse-prices
  "Pull £ amounts out of text. Returns {:price-min :price-max} or nil.
  'Free' with no amounts gives 0."
  [s]
  (when s
    (let [amts (->> (re-seq #"£\s?(\d+(?:\.\d{1,2})?)" s)
                    (map (comp parse-double second)))]
      (cond
        (seq amts) {:price-min (apply min amts) :price-max (apply max amts)}
        (re-find #"(?i)\bfree\b" s) {:price-min 0.0 :price-max 0.0}))))

(defn slug-id
  "A stable id from a URL path."
  [url]
  (-> (str url)
      (str/replace #"^https?://[^/]+" "")
      (str/replace #"^/|/$" "")
      (str/replace #"[?#].*" "")))
