(ns whats-on.bookings
  "Pull past ticket bookings out of Thunderbird, as labelled examples for
  testing the suggestions. Reads a copy of Thunderbird's search index (gloda,
  which holds each message's text), parses the receipts of the box offices
  you've booked through, fills in Headfirst bookings' descriptions from their
  API, and writes one row per booked event to the `bookings` table.

  Only event fields are kept: no addresses, card details or ticket codes."
  (:require [babashka.cli :as cli]
            [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [pod.babashka.go-sqlite3 :as sqlite]
            [whats-on.http :as http]
            [whats-on.util :as u])
  (:import [java.time Instant LocalDate]))

;;; --- dates ------------------------------------------------------------------

(def ^:private months
  (zipmap ["jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"] (range 1 13)))

(defn- ->date
  "LocalDate from day / month name / year strings; nil if they don't make one."
  [d mon y]
  (let [m (months (some-> mon str/lower-case (subs 0 (min 3 (count mon)))))
        y (parse-long (str y))]
    (when (and m y)
      (try (LocalDate/of (if (< y 100) (+ 2000 y) y) (int m) (int (parse-long d)))
           (catch Exception _ nil)))))

(defn- infer-year
  "Receipts that give only day and month: the first such date from (about)
  when the email was sent."
  [d mon ^LocalDate sent]
  (let [this (->date d mon (.getYear sent))]
    (if (and this (.isBefore this (.minusDays sent 7)))
      (->date d mon (inc (.getYear sent)))
      this)))

(defn- hhmm
  "'8:40PM' / '19:30' / '12.00' → 'HH:MM'."
  [s]
  (when-let [[_ h m ap] (re-find #"(?i)(\d{1,2})[:.](\d{2})\s*([ap]m)?" (str s))]
    (let [h (parse-long h)
          h (cond (nil? ap) h
                  (= "pm" (str/lower-case ap)) (if (= 12 h) 12 (+ 12 h))
                  :else (if (= 12 h) 0 h))]
      (format "%02d:%s" h m))))

;;; --- receipt parsers ------------------------------------------------------
;; Each takes {:subject :author :text :raw :sent} and returns booking maps:
;; {:title :venue :event-date :event-time :qty :total :date-approx? ...}

(defn- lines [text] (->> (str/split-lines (str text)) (map str/trim) (remove str/blank?) vec))

(defn- money [s] (some-> (re-find #"£\s*([\d,]+\.\d{2})" (str s)) second (str/replace "," "") parse-double))

(defn- headfirst-ld
  "The schema.org EventReservation newer Headfirst receipts carry."
  [raw]
  (some (fn [el]
          (let [j (try (json/parse-string (.data el) true) (catch Exception _ nil))]
            (when (= "EventReservation" (:type j (get j (keyword "@type")))) j)))
        (u/sel (u/parse-html raw "") "script[type=application/ld+json]")))

(defn- headfirst [{:keys [raw text subject sent]}]
  (let [ld (headfirst-ld raw)
        ev (:reservationFor ld)
        flat (u/clean text)
        [_ t d mon] (re-find #"tickets? for (.+?) on \w+day (\d{1,2})\w* (\w+)" (str flat))
        ;; the event URL's slug starts with the date: /whats-on/<venue>/sun-19-jul-cast-light-157303
        [_ sd smon] (re-find #"headfirstbristol\.co\.uk/whats-on/[^/\"'\s]+/\w{3}-(\d{1,2})-(\w{3})-" raw)
        id (or (some->> (:url ev) (re-find #"-(\d+)$") second)
               (second (re-find #"[#&;]event_id=(\d+)" raw))
               (second (re-find #"#e(\d+)" raw))
               (second (re-find #"headfirstbristol\.co\.uk/whats-on/[^\"'\s]*?-(\d+)[\"'#\s]" raw)))
        date (or (some-> (:startDate ev) (subs 0 10) LocalDate/parse)
                 (some-> (re-find #"#date=(\d{4}-\d{2}-\d{2})" raw) second LocalDate/parse)
                 (when d (infer-year d mon sent))
                 (when sd (infer-year sd smon sent)))]
    ;; the subject's title is often cut short ('La Bomba with...'), but the API fills it in
    [{:title (or (u/clean (:name ev)) (u/clean t)
                 (some->> (re-find #"^Headfirst — (.+?)(?:…|\.\.\.)? \(\d+ x" subject) second u/clean))
      :venue (u/clean (get-in ev [:location :name]))
      :event-date date
      :event-time (some-> (:startDate ev) (subs 11 16))
      :qty (or (some-> (re-find #"\((\d+) x etickets?\)" subject) second parse-long) 1)
      :headfirst-id (some-> id parse-long)
      :url (:url ev)}]))

(def ^:private spektrix-venues
  [[#"bristolbeacon" "Bristol Beacon"] [#"colstonhall" "Colston Hall"]
   [#"stgeorges" "St George's Bristol"] [#"oldvic" "Bristol Old Vic"]])

(defn- house-venue [author]
  (some (fn [[re v]] (when (re-find re (str/lower-case author)) v)) spektrix-venues))

(defn- spektrix-order
  "'Confirmation of your order from …' — Beacon, Old Vic, St George's, TFT."
  [{:keys [text author]}]
  (let [ls (lines text)]
    (keep-indexed
     (fn [i l]
       (when-let [[_ t d mon y tm room] (re-matches #"(.+?) on (?:\w+day )?(\d{1,2}) (\w+) (\d{4}) at (\d{1,2}:\d{2})(?: in (.+))?" l)]
         (let [ticket-lines (take-while #(re-find #"^\d+ .*tickets? .*at £" %) (drop (inc i) ls))]
           {:title t
            :venue (or (house-venue author) room)
            :room room
            :event-date (->date d mon y)
            :event-time tm
            :qty (when (seq ticket-lines) (reduce + (map #(parse-long (re-find #"^\d+" %)) ticket-lines)))
            :total (when (seq ticket-lines)
                     (reduce + (map #(* (parse-long (re-find #"^\d+" %)) (or (money %) 0)) ticket-lines)))})))
     ls)))

(defn- spektrix-etickets
  "'Your E-Tickets are ready' (Old Vic): a title line, then 'Tuesday 03 November 2026, 19:30'."
  [{:keys [text author]}]
  (let [ls (lines text)]
    (keep-indexed
     (fn [i l]
       (when-let [[_ d mon y tm] (re-matches #"\w+day (\d{1,2}) (\w+) (\d{4}), (\d{1,2}:\d{2})" l)]
         (when (pos? i)
           {:title (ls (dec i)) :venue (house-venue author)
            :event-date (->date d mon y) :event-time tm})))
     ls)))

(defn- tickets-com
  "The old (2009–13) Tickets.com receipts used by Watershed, St George's and the
  Tobacco Factory: title, venue, …, 'Tuesday ,  21/5/13 , 8:40PM'."
  [{:keys [text]}]
  (let [ls (lines text)]
    (keep-indexed
     (fn [i l]
       (when-let [[_ d m y tm] (re-find #"^\w+day\s*,\s*(\d{1,2})/(\d{1,2})/(\d{2,4})\s*,\s*(\S+)" l)]
         (let [block (->> (subvec ls 0 i) reverse (take-while #(not (re-find #"^-{10,}" %))) reverse)
               after (take-while #(not (re-find #"^-{10,}" %)) (drop (inc i) ls))
               [_ n total] (some #(re-find #"Price for (\d+) seats? (£[\d.,]+)" %) after)]
           {:title (some-> (first block) (str/replace #"\s*\((U|PG|12A?|15|18)\)\s*\w?$" "") (str/replace #"\s+" " "))
            :venue (second block)
            :event-date (try (LocalDate/of (let [y (parse-long y)] (if (< y 100) (+ 2000 y) y))
                                           (int (parse-long m)) (int (parse-long d)))
                             (catch Exception _ nil))
            :event-time (hhmm tm)
            :qty (some-> n parse-long)
            :total (money total)})))
     ls)))

(defn- atg [{:keys [text]}]
  (let [ls (lines text)]
    (keep-indexed
     (fn [i l]
       ;; 'Wednesday, 11 November 2026', or before 2024 'Wed 16 Aug 2023 19:30'
       (when-let [[_ d mon y tm] (or (re-matches #"\w+day, (\d{1,2}) (\w+) (\d{4})" l)
                                     (re-matches #"\w{3} (\d{1,2}) (\w{3}) (\d{4}) (\d{1,2}:\d{2})" l))]
         (when (> i 1)
           {:title (ls (- i 2)) :venue (ls (dec i)) :event-date (->date d mon y)
            :event-time (or tm (some->> (re-find #"Show starts (\d{1,2}:\d{2})" text) second))
            :total (some->> (re-find #"Order total\s+(£[\d.,]+)" text) second money)})))
     ls)))

(defn- see-tickets [{:keys [text]}]
  (let [[_ t v] (re-find #"Booking confirmation for (.+) at (.+)" text)
        [_ d mon y tm] (or (re-find #"\w+day, (\d{1,2}) (\w{3,}) (\d{4}) at (\d{1,2}[.:]\d{2})" text)
                           ;; multi-day, 2016: 'Thu, 10 Mar at 7:00 PM - Sat, 12 Mar 2016 at 11:00 PM'
                           (let [[_ d mon tm y] (re-find #"\w{3}, (\d{1,2}) (\w{3}) at (\d{1,2}:\d{2} [AP]M) - \w{3}, \d{1,2} \w{3} (\d{4})" text)]
                             (when d [nil d mon y tm])))]
    (when t
      [{:title t :venue v :event-date (when d (->date d mon y)) :event-time (hhmm tm)
        :qty (some->> (re-seq #"(?m)^(\d+) x (?!Order)" text) (map (comp parse-long second)) (reduce +))
        :total (some->> (re-find #"Total\s+(£[\d.,]+)" text) second money)}])))

(defn- skiddle [{:keys [subject text]}]
  (let [t (second (re-find #"Your ticket order for (.+?) \(Order" subject))
        [_ d mon y] (re-find #"\(\w{3} (\d{1,2})\w* (\w+) (\d{4})\)" text)]
    (when t
      [{:title t :event-date (when d (->date d mon y))
        :qty (some->> (re-seq #"(?m)^(\d+)x " text) (map (comp parse-long second)) (reduce +))
        :total (some->> (re-find #"Grand Total:\s+(£[\d.,]+)" text) second money)}])))

(defn- eventbrite
  "Receipts don't reliably state the event date, so it's the email date (approx)."
  [{:keys [subject text sent]}]
  (when-let [t (second (re-find #"(?:Your Tickets for|Order Confirmation for|Registration Confirmation for) (.+)" subject))]
    [{:title t :venue (second (re-find #"Follow (.+?): https" text))
      :event-date sent :date-approx? true
      :qty (some->> (re-find #"(\d+) x Tickets?" text) second parse-long)}]))

(defn- ticketsource [{:keys [text]}]
  (let [after #(second (re-find (re-pattern (str "(?m)^" % "\\s*\\n\\s*(\\S.*)$")) text))
        [_ d mon y tm] (re-find #"\w+day (\d{1,2})\w* (\w+) (\d{4}),? ?([\d:]+\s*[AP]M)?" (str (after "Date")))]
    (when-let [t (after "Event")]
      [{:title t :venue (some-> (after "Venue") (str/replace #"([a-z])([A-Z])" "$1, $2"))
        :event-date (when d (->date d mon y)) :event-time (hhmm tm)
        :total (some->> (after "Total Price") money)}])))

(defn- trybooking [{:keys [text]}]
  (let [ls (lines text)]
    (keep-indexed
     (fn [i l]
       (when-let [[_ d mon y tm] (re-matches #"(\d{1,2}) (\w+) (\d{4}) (\d{1,2}:\d{2} [AP]M).*" l)]
         (when (and (pos? i) (not (re-find #"(?i)date booked" (ls (dec i)))))
           {:title (ls (dec i)) :venue (get ls (inc i))
            :event-date (->date d mon y) :event-time (hhmm tm)})))
     ls)))

(defn- subject-only
  "Receipts whose bodies are images or PDFs: the title from the subject, dated by the email."
  [re]
  (fn [{:keys [subject sent]}]
    (when-let [t (second (re-find re subject))]
      [{:title t :event-date sent :date-approx? true}])))

(def templates
  "What to look for: sender (SQL LIKE on the From header), a subject regex, and a parser.
  Senders not listed here are not read at all."
  [{:vendor "headfirst"    :from "%headfirstbristol%"      :subject #"^Headfirst — .*eticket"          :parse headfirst}
   {:vendor "spektrix"     :from "%bristolbeacon.org%"     :subject #"^Confirmation of your order"     :parse spektrix-order}
   {:vendor "spektrix"     :from "%stgeorgesbristol%"      :subject #"^Confirmation of your order"     :parse spektrix-order}
   {:vendor "spektrix"     :from "%bristololdvic%"         :subject #"^Confirmation of your order"     :parse spektrix-order}
   {:vendor "spektrix"     :from "%bristololdvic%"         :subject #"^Your E-Tickets are ready"       :parse spektrix-etickets
    ;; some have no details, but then there's an order confirmation too
    :optional? true}
   {:vendor "spektrix"     :from "%tobaccofactorytheatre%" :subject #"^Confirmation of your order"     :parse spektrix-order}
   {:vendor "spektrix"     :from "%colstonhall%"           :subject #"^Confirmation of your order"     :parse spektrix-order}
   {:vendor "tickets.com"  :from "%watershed%"             :subject #"(?i)^Your Ticket Purchase"       :parse tickets-com}
   {:vendor "tickets.com"  :from "%stgeorgesbristol%"      :subject #"(?i)^Your Ticket Purchase"       :parse tickets-com}
   {:vendor "tickets.com"  :from "%tobaccofactory%"        :subject #"(?i)^Your Ticket Purchase"       :parse tickets-com}
   {:vendor "atg"          :from "%atgtickets%"            :subject #"^Your ATG Tickets order"         :parse atg}
   {:vendor "seetickets"   :from "%seetickets%"            :subject #"(?i)ticket"                      :parse see-tickets}
   {:vendor "skiddle"      :from "%skiddle%"               :subject #"^Your ticket order for"          :parse skiddle}
   {:vendor "eventbrite"   :from "%eventbrite%"            :subject #"(?:Your Tickets|Order Confirmation|Registration Confirmation) for" :parse eventbrite}
   {:vendor "ticketsource" :from "%ticketsource%"          :subject #"^Booking Confirmation"           :parse ticketsource}
   {:vendor "trybooking"   :from "%trybooking%"            :subject #"^Your booking confirmation"      :parse trybooking}
   {:vendor "kaboodle"     :from "%kaboodle%"              :subject #"^Thank you for booking"          :parse (subject-only #"Thank you for booking (.+?)!")}
   {:vendor "ambassador"   :from "%ambassadortickets%"     :subject #"(?i)confirm|your tickets|booking" :parse (subject-only #"(?i)(?:for|:) (.+)$")}
   ;; Watershed / Colston Hall 2017–22: the details are only in the PDF, so these are reported, not parsed
   {:vendor "patronbase"   :from "%patronbase%"            :subject #"Sale Receipt"                    :parse (constantly nil)}])

;;; --- reading the mail index ---------------------------------------------------

(defn- default-profile []
  (let [base (fs/path (System/getProperty "user.home") ".thunderbird")
        ini (fs/file base "profiles.ini")]
    (when (fs/exists? ini)
      (some->> (slurp ini) (re-find #"(?m)^Path=(.+)$") second str/trim (fs/path base) str))))

(defn- copy-index
  "Thunderbird holds the index open, so read a private copy."
  [profile]
  (let [dir (fs/create-temp-dir {:prefix "whats-on-gloda"})]
    (doseq [f (fs/glob profile "global-messages-db.sqlite*")]
      (fs/copy f (fs/path dir (fs/file-name f))))
    (str (fs/path dir "global-messages-db.sqlite"))))

(defn- messages [index account]
  (let [folders (sqlite/query index ["select id from folderLocations where folderURI like ? and folderURI not like '%Junk%'"
                                     (str "%" account "%")])
        froms (distinct (map :from templates))]
    (when (empty? folders)
      (throw (ex-info (str "No folders for account " account) {})))
    (sqlite/query index (into [(str "select m.headerMessageID as message_id, m.date as date,
                                            t.c1subject as subject, t.c3author as author, t.c0body as body
                                       from messages m join messagesText_content t on t.docid = m.id
                                      where m.deleted = 0
                                        and m.folderID in (" (str/join "," (map :id folders)) ")
                                        and (" (str/join " or " (repeat (count froms) "lower(t.c3author) like ?")) ")")]
                              froms))))

(defn- ->msg [{:keys [subject author body date] :as m}]
  (let [raw (str body)]
    (assoc m
           :subject (str subject) :author (str author) :raw raw
           :text (if (re-find #"(?i)<(html|body|div|table|p)\b" raw) (u/html->text raw) raw)
           :sent (LocalDate/ofInstant (Instant/ofEpochMilli (quot (long date) 1000)) u/london))))

(defn- template-for [{:keys [author subject]}]
  (let [a (str/lower-case author)]
    (some (fn [t] (when (and (str/includes? a (str/replace (:from t) "%" ""))
                             (re-find (:subject t) subject))
                    t))
          templates)))

;;; --- enrichment -------------------------------------------------------------------

(def ^:private headfirst-api "https://www.headfirstbristol.co.uk/api/v2/events")

(defn- headfirst-day
  "All Headfirst events on one date (cached by whats-on.http, so re-runs are free)."
  [^LocalDate d]
  (loop [offset 0 acc []]
    (let [batch (:events (http/fetch-json
                          (http/url headfirst-api
                                    [[:bundle "publicWebsite"]
                                     ["filter[0][field]" "date"] ["filter[0][operator]" "="] ["filter[0][value]" (str d)]
                                     [:orderBy "event_id"] [:orderDirection "asc"]
                                     [:limit 200] [:offset offset]])
                          ;; see the note in sources/headfirst.clj
                          {:ignore-robots? true}))
          acc (into acc batch)]
      (if (= 200 (count batch)) (recur (+ offset 200) acc) acc))))

(defn- norm-title [s] (str/trim (str/replace (str/lower-case (str s)) #"[^a-z0-9]+" " ")))

(defn- title-match? [a b]
  (let [a (norm-title a) b (norm-title b)]
    (and (seq a) (seq b) (or (str/starts-with? a b) (str/starts-with? b a)))))

(defn- enrich-headfirst [bookings]
  (let [days (->> bookings (filter #(and (= "headfirst" (:vendor %)) (:event-date %))) (map :event-date) distinct sort)
        _ (u/log (format "Fetching %d days of Headfirst listings for descriptions…" (count days)))
        by-day (into {} (map (fn [d] [d (try (headfirst-day d)
                                             (catch Exception e (u/log "  " (str d) "failed:" (ex-message e)) []))]))
                     days)]
    (for [b bookings]
      (if-let [e (and (= "headfirst" (:vendor b))
                      (let [evs (by-day (:event-date b))]
                        (or (some #(when (= (:headfirst-id b) (:event_id %)) %) evs)
                            (some #(when (title-match? (:title b) (:event_name %)) %) evs))))]
        (merge b
               {:title (or (u/clean (:event_name e)) (:title b))
                :venue (or (u/clean (:venue_name e)) (:venue b))
                :headfirst-id (:event_id e)
                :subtitle (u/clean (:desc_short e))
                :description (u/clean-paragraphs (:desc_long e))
                :category (:type e)
                :tags (->> (:subGenres e) (keep (comp u/clean :subGenreName)) distinct vec)
                :event-time (or (:event-time b) (:start_time e) (:doors_time e))
                :url (or (:url b) (str "https://www.headfirstbristol.co.uk/#e" (:event_id e)))})
        b))))

(defn- enrich-from-events
  "Anything still without a description: the scraper's own (past) listings, if it saw the event."
  [dbf bookings]
  (for [b bookings]
    (if (or (:description b) (:date-approx? b) (nil? (:event-date b)))
      b
      (if-let [e (some #(when (title-match? (:title b) (:title %)) %)
                       (sqlite/query dbf ["select title, venue, subtitle, description, category, tags, url from events
                                            where start_date <= ? and coalesce(end_date, start_date) >= ?
                                              and description is not null"
                                          (str (:event-date b)) (str (:event-date b))]))]
        (merge b {:subtitle (:subtitle e) :description (:description e) :category (:category e)
                  :tags (try (json/parse-string (:tags e)) (catch Exception _ []))
                  :url (or (:url b) (:url e)) :venue (or (:venue b) (:venue e))})
        b))))

;;; --- storage ------------------------------------------------------------------

(def schema
  "create table if not exists bookings (
     key          text primary key,   -- normalised title | event date
     vendor       text not null,      -- which receipt format it came from
     title        text not null,
     venue        text,
     room         text,
     event_date   text,
     event_time   text,
     date_approx  integer not null default 0,  -- 1: only the email date is known
     qty          integer,
     total        real,
     email_date   text,
     headfirst_id integer,
     url          text,
     subtitle     text,
     description  text,
     category     text,
     tags         text,               -- JSON array
     message_id   text,               -- Message-ID of the receipt
     extracted_at text not null)")

(def ^:private columns
  [:key :vendor :title :venue :room :event_date :event_time :date_approx :qty :total :email_date
   :headfirst_id :url :subtitle :description :category :tags :message_id :extracted_at])

(defn- ->row [now b]
  {:key (str (norm-title (:title b)) "|" (:event-date b))
   :vendor (:vendor b) :title (:title b) :venue (:venue b) :room (:room b)
   :event_date (some-> (:event-date b) str) :event_time (:event-time b)
   :date_approx (if (:date-approx? b) 1 0) :qty (:qty b) :total (:total b)
   :email_date (str (:sent b)) :headfirst_id (:headfirst-id b) :url (:url b)
   :subtitle (:subtitle b) :description (:description b) :category (:category b)
   :tags (json/generate-string (vec (:tags b))) :message_id (:message_id b) :extracted_at now})

(defn- save! [dbf rows]
  (sqlite/execute! dbf [schema])
  (doseq [r rows]
    (sqlite/execute! dbf (into [(str "insert into bookings (" (str/join ", " (map name columns)) ") values ("
                                     (str/join ", " (repeat (count columns) "?")) ")"
                                     " on conflict(key) do update set "
                                     (str/join ", " (map #(str (name %) " = excluded." (name %)) (rest columns))))]
                               (map r columns)))))

;;; --- main -------------------------------------------------------------------------

(defn- merge-dupes
  "A booking often arrives as an order confirmation plus a tickets email; keep
  one row per event, filling gaps from the others."
  [bookings]
  (->> bookings
       (group-by (juxt #(norm-title (:title %)) :event-date))
       vals
       (map (fn [bs] (reduce (fn [a b] (merge-with #(if (nil? %1) %2 %1) a b))
                             (sort-by #(if (:date-approx? %) 1 0) bs))))))

(defn run [& args]
  (let [opts (cli/parse-opts args {:coerce {:profile :string :account :string}})
        cfg (merge {:db "whats-on.db"} (when (fs/exists? "config.edn") (edn/read-string (slurp "config.edn"))))
        profile (or (:profile opts) (default-profile) (throw (ex-info "No Thunderbird profile found; pass --profile" {})))
        account (or (:account opts) "imap.fastmail.com")
        dbf (:db cfg)
        _ (swap! http/config merge (select-keys cfg [:delay-ms :contact]))
        index (copy-index profile)
        msgs (try (mapv ->msg (messages index account))
                  (finally (fs/delete-tree (fs/parent index))))
        parsed (for [m msgs :let [t (template-for m)] :when t]
                 (let [bs (try ((:parse t) m) (catch Exception e (u/log "parse failed:" (:subject m) (ex-message e)) nil))
                       bs (filter #(and (:title %) (:event-date %)) bs)]
                   {:msg m :vendor (:vendor t) :optional? (:optional? t)
                    :bookings (map #(assoc % :vendor (:vendor t) :sent (:sent m) :message_id (:message_id m)) bs)}))
        unparsed (filter #(and (empty? (:bookings %)) (not (:optional? %))) parsed)
        bookings (->> (mapcat :bookings parsed)
                      merge-dupes
                      enrich-headfirst
                      (enrich-from-events dbf)
                      (sort-by :event-date))
        now (str (Instant/now))]
    (save! dbf (map #(->row now %) bookings))
    (u/log (format "\n%d messages from known senders, %d receipts, %d booked events → bookings table in %s"
                   (count msgs) (count parsed) (count bookings) dbf))
    (doseq [[v bs] (sort-by key (group-by :vendor bookings))]
      (u/log (format "  %-13s %3d  (%d with description, %d with approximate date)"
                     v (count bs) (count (filter :description bs)) (count (filter :date-approx? bs)))))
    (when (seq unparsed)
      (u/log (format "\n%d receipts couldn't be parsed:" (count unparsed)))
      (doseq [{:keys [vendor msg]} (sort-by (comp :sent :msg) unparsed)]
        (u/log (format "  %s %-11s %s" (:sent msg) vendor (:subject msg)))))))
