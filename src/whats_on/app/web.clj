(ns whats-on.app.web
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup2.core :as h]
            [whats-on.app.embed :as embed]
            [whats-on.app.rank :as rank]
            [whats-on.app.store :as store])
  (:import [java.time DayOfWeek LocalDate]
           [java.time.format DateTimeFormatter]
           [java.time.temporal TemporalAdjusters]))

(defonce status (atom {:embedded 0 :total 0}))

;;; --- helpers --------------------------------------------------------------

(defn- today [] (LocalDate/now))
(defn- ->date [s] (try (LocalDate/parse (str s)) (catch Exception _ nil)))
(defn- fmt [^LocalDate d pattern] (.format d (DateTimeFormatter/ofPattern pattern)))

(defn- dates-in
  "The item's performances that fall in [from, to] (inclusive)."
  [item from to]
  (filter #(and (<= (compare (:start_date %) (str to)) 0)
                (>= (compare (or (:end_date %) (:start_date %)) (str from)) 0))
          (:dates item)))

(defn- time-ok? [time-of-day {:keys [start_time]}]
  (case time-of-day
    "evening" (or (nil? start_time) (>= (compare start_time "17:00") 0))
    "daytime" (or (nil? start_time) (neg? (compare start_time "17:00")))
    true))

(defn- price-str [{:keys [price_min price_max price_text]}]
  (cond
    (and price_min (zero? price_min) (or (nil? price_max) (zero? price_max))) "Free"
    (and price_min price_max (not= price_min price_max)) (format "£%.0f–£%.0f" price_min price_max)
    price_min (format "£%.2f" price_min)
    :else price_text))

(defn- date-str [{:keys [start_date start_time end_date]}]
  (let [d (->date start_date)]
    (str (fmt d "EEE d MMM")
         (when start_time (str " " start_time))
         (when-let [e (->date end_date)] (str " – " (fmt e "EEE d MMM"))))))

(defn- all-sold-out? [dates] (and (seq dates) (every? #(= 1 (:sold_out %)) dates)))

(defn- short-id [key] (subs (store/sha1 key) 0 12))

;;; --- layout -----------------------------------------------------------------

(defn- css-version
  "Changes whenever app.css does, so browsers don't keep a stale copy."
  []
  (some-> (io/resource "public/static/app.css") .openConnection .getLastModified))

(defn- page [title active & body]
  (str
   (h/html
    (h/raw "<!doctype html>")
    [:html {:lang "en-GB"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:title (str title " · What's on")]
      [:link {:rel "stylesheet" :href (str "/static/app.css?v=" (css-version))}]
      [:script {:src "/static/htmx.min.js" :defer true}]]
     [:body
      [:header
       [:nav
        (for [[href label] [["/plan" "Plan ahead"] ["/find" "Find something"] ["/rate" "Help me learn"] ["/taste" "My taste"]]]
          [:a {:href href :aria-current (when (= href active) "page")} label])]
       [:div#status {:hx-get "/status" :hx-trigger "load, every 5s" :hx-swap "innerHTML"}]]
      [:main [:h1 title] body]]])))

;;; --- event cards ----------------------------------------------------------

(def verdict-labels (array-map "love" "⭐ Must go" "booked" "✓ Booked" "like" "👍 Interested"
                                "meh" "😐 Meh" "dislike" "👎 Not for me"))

(def ^:private rating-buttons
  ;; verdict, button, tooltip; in the order shown
  [["dislike" "👎" "Not for me"] ["meh" "😐" "Meh"] ["like" "👍" "Interested"] ["love" "⭐" "Must go"]])

(defn- feedback-button
  "`view` is \"card\" or \"row\": the server sends back the same kind of element to swap in."
  ([view key verdict current label] (feedback-button view key verdict current label nil))
  ([view key verdict current label tip]
   [:button {:hx-post "/feedback"
             :hx-vals (json/generate-string {:key key :view view :verdict (if (= current verdict) "none" verdict)})
             :hx-target "closest .item" :hx-swap "outerHTML"
             :aria-pressed (str (= current verdict))
             :title tip :aria-label tip}
    label]))

(defn- hide-button
  "Hiding is separate from rating: it only says 'don't show me this'."
  [view key hidden? label]
  [:button {:hx-post "/hide"
            :hx-vals (json/generate-string {:key key :view view :hidden (not hidden?)})
            :hx-target "closest .item" :hx-swap "outerHTML"
            :aria-pressed (str (boolean hidden?))
            :title (if hidden? "Show again" "Don't show me this") :aria-label (if hidden? "Unhide" "Hide")}
   label])

(defn- similar-href [key] (str "/find?like=" (java.net.URLEncoder/encode key "UTF-8")))

(defn- why-text [{:keys [because unlike title]}]
  ;; an event you rated is trivially its own best match; that says nothing
  (let [because (when-not (and because (str/starts-with? (:label because) (str title))) because)]
    (not-empty
     (str (when because (str "Because " (:why because) " " (:label because) ". "))
          (when unlike (str "Though it's like " (:label unlike) ", which you didn't fancy."))))))

(defn- dismissed-card [item verdict]
  [:article.card.item.dismissed {:id (str "c-" (short-id (:key item)))}
   (if (:hidden? item)
     [:p "Hidden: " (:title item) " " (hide-button "card" (:key item) true "Undo")]
     [:p (verdict-labels verdict) ": " (:title item) " "
      (feedback-button "card" (:key item) verdict verdict "Undo")])])

(declare extra-links)

(defn card
  "One event. `shown-dates` are the performances relevant to the current view.
  An item with :hidden? set (just hidden) shows collapsed, as does a dislike."
  ([item verdict] (card item verdict (:dates item)))
  ([item verdict shown-dates]
   (if (or (:hidden? item) (= "dislike" verdict))
     (dismissed-card item verdict)
     (let [{:keys [key title subtitle venue description url]} item
           shown (take 4 shown-dates)
           more (- (count shown-dates) (count shown))
           first-booking (some :booking_url shown)
           paras (remove str/blank? (str/split (or description "") #"\n\n+"))
           why (or (:learn-why item) (why-text item))]
       [:article.card.item {:id (str "c-" (short-id key))}
        [:div.head
         [:h2 [:a {:href (or first-booking url)} title]
          (when subtitle [:span.sub " " subtitle])
          (when verdict [:span.badge (verdict-labels verdict)])]
         [:div.actions
          (for [[v label tip] rating-buttons] (feedback-button "card" key v verdict label tip))
          (feedback-button "card" key "booked" verdict "Booked")
          (hide-button "card" key false "Hide")
          [:a.more {:href (similar-href key)} "Similar"]]]
        [:p.meta
         [:span.when (str/join ", " (map date-str shown))
          (when (pos? more) (str " (+" more " more)"))]
         (when (all-sold-out? shown) [:strong " · sold out"])
         (str/join "" (map #(str " · " %) (remove str/blank? [venue (rank/kind-labels (rank/kind item)) (price-str item)])))]
        (when why [:p.why why])
        (when (or (seq paras) (seq (:tags item)) (:also_listed item))
          [:details
           [:summary [:span.snip (or (first paras) "Details")]]
           (for [p paras] [:p p])
           (extra-links item)])]))))

(defn- extra-links [{:keys [tags url also_listed]}]
  (list
   (when (seq tags) [:p.tags (str "Tags: " (str/join ", " tags))])
   [:p.links
    (when url [:a {:href url} "Event page"])
    (for [line (str/split-lines (or also_listed ""))
          :let [[src link] (str/split line #" " 2)]
          :when link]
      (list " · " [:a {:href link} (str "also on " src)]))]))

(defn- short-date [{:keys [start_date start_time end_date]}]
  (let [d (->date start_date)]
    (str (fmt d "EEE d MMM")
         (when start_time (str " " start_time))
         (when-let [e (->date end_date)] (str "–" (fmt e "d MMM"))))))

(defn row
  "One event for the dense Find results: its own <tbody>, holding the event's
  line and a full-width details line underneath, folded away until the
  'details' toggle opens it (see details-script). Buttons swap the whole tbody."
  ([item verdict] (row item verdict (:dates item)))
  ([item verdict shown-dates]
   (let [{:keys [key title subtitle venue description url rank qsim window]} item
         attrs {:id (str "r-" (short-id key)) :hx-vals (when window (json/generate-string window))}]
     (if (or (:hidden? item) (= "dislike" verdict))
       [:tbody.item.dismissed attrs
        [:tr
         (if (:hidden? item)
           [:td {:colspan 12} "Hidden: " title " " (hide-button "row" key true "Undo")]
           [:td {:colspan 12} (verdict-labels verdict) ": " title " "
            (feedback-button "row" key verdict verdict "Undo")])]]
       (let [paras (remove str/blank? (str/split (or description "") #"\n\n+"))
             why (why-text item)
             more (dec (count shown-dates))]
         [:tbody.item attrs
          [:tr
           [:td.when (short-date (first shown-dates))
            (when (pos? more) [:span.more-dates (str " +" more)])
            (when (all-sold-out? shown-dates) [:strong " sold out"])]
           [:td.ev
            [:a {:href (or (some :booking_url shown-dates) url)} title]
            (when subtitle [:span.sub " " subtitle])
            (when verdict [:span.badge (verdict-labels verdict)])
            [:button.toggle {:type "button" :aria-expanded "false"} "details"]]
           [:td venue]
           [:td (rank/kind-labels (rank/kind item))]
           [:td.price (price-str item)]
           ;; one column per action, so the buttons line up down the table
           (for [[v label tip] rating-buttons] [:td.act (feedback-button "row" key v verdict label tip)])
           [:td.act (feedback-button "row" key "booked" verdict "✓" "Booked")]
           [:td.act (hide-button "row" key false "✕")]
           [:td.act [:a.button {:href (similar-href key) :title "More like this" :aria-label "More like this"} "≈"]]]
          [:tr.detail {:hidden true}
           [:td {:colspan 12}
            (when (pos? more)
              [:p.dates "Dates: " (str/join ", " (map short-date shown-dates))])
            (when (and rank (>= rank 0))
              [:p.why (format "Match %.2f" (double rank))
               (when qsim (format " (search %.2f)" (double qsim)))
               (when why (str ". " why))])
            (for [p paras] [:p p])
            (extra-links item)]]])))))

(def ^:private details-script
  ;; open/close an event's full-width details line in the Find results: the
  ;; 'details' toggle, or a click anywhere else on the event's line except its
  ;; links and buttons (and not when selecting text)
  "document.addEventListener('click', e => {
     const line = e.target.closest('.results tbody.item > tr:first-child');
     if (!line) return;
     const b = line.querySelector('button.toggle');
     const t = e.target.closest('a, button, input, select');
     if (!b || (t && t !== b) || String(getSelection())) return;
     const d = line.parentElement.querySelector('tr.detail');
     d.hidden = !d.hidden;
     b.setAttribute('aria-expanded', String(!d.hidden));
     b.textContent = d.hidden ? 'details' : 'less';
   });")

;;; --- context shared by the pages ------------------------------------------

(defn- ctx [ds]
  (let [examples (vec (rank/examples ds))]
    {:items (store/items ds)
     :verdicts (store/feedback-map ds)
     :hidden (store/hidden-keys ds)
     :examples examples
     :model (rank/model ds examples)}))

(defn- no-taste-note [examples]
  (when-not (some #(pos? (:weight %)) examples)
    [:p.note "No taste data yet, so these aren't personalised. "
     [:a {:href "/rate"} "Rate a few events"] " or "
     [:a {:href "/taste"} "tell it what you like"] " to get suggestions."]))

;;; --- Plan ahead -------------------------------------------------------------

(def plan-lead-days 21)
(def plan-excluded-kinds #{:film :exhibition})

(defn plan-page [ds _params]
  (let [{:keys [items verdicts hidden examples model]} (ctx ds)
        from (.plusDays (today) plan-lead-days)
        candidates (->> items
                        (remove #(plan-excluded-kinds (rank/kind %)))
                        (remove #(#{"dislike" "meh" "booked"} (verdicts (:key %))))
                        (remove #(hidden (:key %)))
                        ;; worth booking ahead: a ticketed one-off or short run
                        (remove #(and (:price_max %) (zero? (:price_max %))))
                        (remove #(some (fn [d] (when-let [e (->date (:end_date d))]
                                                 (> (.toEpochDay e) (+ 14 (.toEpochDay (->date (:start_date d)))))))
                                       (:dates %)))
                        (keep #(let [ds* (dates-in % from (.plusYears from 1))]
                                 (when (and (seq ds*) (not (all-sold-out? ds*)))
                                   (assoc % :shown ds*)))))
        scored (rank/score-items ds candidates {:model model})
        by-month (group-by #(subs (:start_date (first (:shown %))) 0 7) scored)
        booked (->> items (filter #(= "booked" (verdicts (:key %)))) (sort-by #(:start_date (first (:dates %)))))]
    (page "Plan ahead" "/plan"
          [:p.intro (str "The best matches from " (fmt from "d MMMM")
                         " onwards: ticketed, one-off or short-run things worth booking early. "
                         "Films and exhibitions are left out here; use Find for those.")]
          (no-taste-note examples)
          (when (seq booked)
            [:section [:h2 "Booked"]
             [:ul.booked (for [b booked]
                           [:li (date-str (first (:dates b))) " — " [:a {:href (or (:booking_url (first (:dates b))) (:url b))} (:title b)] " at " (:venue b)])]])
          (for [[month evs] (sort-by key by-month)
                :let [ranked (sort-by :rank > evs)
                      label (fmt (->date (str month "-01")) "MMMM yyyy")]]
            [:section
             [:h2 label]
             [:h3.pick "Top picks"]
             (for [e (take 2 ranked)] (card e (verdicts (:key e)) (:shown e)))
             (when (> (count ranked) 2)
               [:details.also
                [:summary (str "Also worth a look in " label)]
                (for [e (take 6 (drop 2 ranked))] (card e (verdicts (:key e)) (:shown e)))])]))))

;;; --- Find ---------------------------------------------------------------------

(defn- weekend-from [^LocalDate d]
  (if (#{DayOfWeek/SATURDAY DayOfWeek/SUNDAY} (.getDayOfWeek d))
    d
    (.with d (TemporalAdjusters/next DayOfWeek/SATURDAY))))

(defn- quick-ranges []
  (let [t (today)
        sat (weekend-from t)
        next-sat (.plusDays (if (= DayOfWeek/SUNDAY (.getDayOfWeek sat)) (.minusDays sat 1) sat) 7)]
    [["Today" t t]
     ["Tomorrow" (.plusDays t 1) (.plusDays t 1)]
     ["This weekend" sat (.with sat (TemporalAdjusters/nextOrSame DayOfWeek/SUNDAY))]
     ["Next weekend" next-sat (.plusDays next-sat 1)]
     ["Next 7 days" t (.plusDays t 6)]
     ["Next 30 days" t (.plusDays t 29)]]))

(defn- query-string [params]
  (let [enc #(java.net.URLEncoder/encode (str %) "UTF-8")]
    (str/join "&" (for [[k v] (sort-by key params)
                        v (if (sequential? v) v [v])
                        :when (not (str/blank? (str v)))]
                    (str (enc k) "=" (enc v))))))

(def cal-months 3)

(defn- cal-start
  "First day of the calendar's first month: this month, unless `from` is beyond what it would show."
  [^LocalDate from]
  (let [this-month (.withDayOfMonth (today) 1)]
    (if (.isBefore from (.plusMonths this-month cal-months))
      this-month
      (.withDayOfMonth from 1))))

(defn- day-counts
  "date string → number of matching events that day. Runs (exhibitions etc.)
  are left out; they would add the same amount to every day."
  [items time-of-day include-sold-out?]
  (frequencies
   (mapcat (fn [item]
             (->> (:dates item)
                  (filter #(and (nil? (:end_date %))
                                (time-ok? time-of-day %)
                                (or include-sold-out? (not= 1 (:sold_out %)))))
                  (map :start_date)
                  distinct))
           items)))

(def ^:private range-script
  ;; shift-click a day to select from the current start date to that day
  "document.addEventListener('click', e => {
     const a = e.target.closest('.cal a[data-date]');
     if (!a || !e.shiftKey) return;
     e.preventDefault();
     const start = a.closest('.cals').dataset.from, d = a.dataset.date;
     const u = new URL(a.href, location);
     u.searchParams.set('from', start <= d ? start : d);
     u.searchParams.set('to', start <= d ? d : start);
     location = u;
   });")

(defn- calendar [^LocalDate start ^LocalDate from ^LocalDate to counts href]
  (let [t (today)
        busiest (max 1 (reduce max 0 (vals counts)))]
    [:div.cals {:data-from (str from)}
     (for [m (range cal-months)
           :let [first-day (.plusMonths start m)
                 days (.lengthOfMonth first-day)
                 ;; blank cells before the 1st, Monday-first
                 lead (dec (.getValue (.getDayOfWeek first-day)))
                 cells (concat (repeat lead nil) (map #(.plusDays first-day %) (range days)))]]
       [:table.cal
        [:caption (fmt first-day "MMMM yyyy")]
        [:thead [:tr (for [d ["Mo" "Tu" "We" "Th" "Fr" "Sa" "Su"]] [:th d])]]
        [:tbody
         (for [week (partition 7 7 (repeat nil) cells)]
           [:tr
            (for [^LocalDate d week]
              (if-not d
                [:td]
                (let [n (counts (str d) 0)
                      level (if (zero? n) 0 (int (Math/ceil (/ (* 4.0 n) busiest))))
                      cls (str/join " " (remove nil? [(str "l" level)
                                                      (when (.isBefore d t) "past")
                                                      (when (= d t) "today")
                                                      (when (and (not (.isBefore d from)) (not (.isAfter d to))) "sel")]))]
                  [:td {:class cls}
                   (if (.isBefore d t)
                     [:span (.getDayOfMonth d)]
                     [:a {:href (href d d) :data-date (str d)
                          :title (str (fmt d "EEE d MMM") ": " n (if (= 1 n) " event" " events"))}
                      (.getDayOfMonth d)])])))])]])
     [:p.hint "Click a day to see it; shift-click another to see the range between. Darker days have more on."]
     [:script (h/raw range-script)]]))

(def find-page-size 40)

(declare find-page-html)

(defn find-page [ds params]
  (let [{:keys [items verdicts hidden examples model]} (ctx ds)
        p #(let [v (get params %)] (when-not (str/blank? v) (str/trim v)))
        like-key (p "like")
        like-item (when like-key (first (filter #(= like-key (:key %)) items)))
        from (or (->date (p "from")) (today))
        to (or (->date (p "to")) (if like-item (.plusDays from 120) from))
        time-of-day (or (p "time") "any")
        kinds (set (keep #(keyword %) (let [k (get params "kind")] (if (string? k) [k] k))))
        max-price (some-> (p "max_price") parse-double)
        venue (some-> (p "venue") str/lower-case)
        q (p "q")
        offset (or (some-> (p "offset") parse-long) 0)
        include-sold-out? (= "on" (p "sold_out"))
        query-vec (cond
                    like-item (@store/vectors (:hash like-item))
                    q (store/vector-for ds (str embed/query-prefix q)))
        filtered (->> items
                      (remove #(= "dislike" (verdicts (:key %))))
                      (remove #(hidden (:key %)))
                      (remove #(= like-key (:key %)))
                      (filter #(or (empty? kinds) (kinds (rank/kind %))))
                      (filter #(or (nil? max-price) (nil? (:price_min %)) (<= (:price_min %) max-price)))
                      (filter #(or (nil? venue) (str/includes? (str/lower-case (str (:venue %))) venue))))
        window {:from (str from) :to (str to) :time time-of-day}
        candidates (keep #(let [ds* (filter (partial time-ok? time-of-day) (dates-in % from to))]
                            (when (and (seq ds*) (or include-sold-out? (not (all-sold-out? ds*))))
                              (assoc % :shown ds* :window window)))
                         filtered)
        results (->> (rank/score-items ds candidates {:model model :query-vec query-vec :q q})
                     (sort-by :rank >)
                     (drop offset)
                     (take find-page-size))
        ;; infinite scroll: the last row fetches the next page when it comes into view
        rows (concat (for [e results] (row e (verdicts (:key e)) (:shown e)))
                     (when (< (+ offset find-page-size) (count candidates))
                       [[:tbody.more {:hx-get (str "/find?" (query-string (assoc params "offset" (+ offset find-page-size)
                                                                                 "fragment" "rows")))
                                      :hx-trigger "revealed" :hx-swap "outerHTML"}
                         [:tr [:td {:colspan 12} "Loading more…"]]]]))
        checked? #(contains? kinds %)
        ;; the calendar keeps every other filter and only changes the dates
        keep-params (dissoc params "from" "to" "offset" "fragment")
        href (fn [f t] (str "/find?" (query-string (assoc keep-params "from" (str f) "to" (str t)))))]
    (if (p "fragment")
      (str (h/html rows))
      (find-page-html {:like-item like-item :like-key like-key :from from :to to :time-of-day time-of-day
                       :checked? checked? :href href :p p :include-sold-out? include-sold-out?
                       :filtered filtered :examples examples :candidates candidates :rows rows}))))

(defn- find-page-html
  [{:keys [like-item like-key from to time-of-day checked? href p include-sold-out?
           filtered examples candidates rows]}]
  (let [q (p "q")]
    (page (if like-item (str "More like “" (:title like-item) "”") "Find something") "/find"
          [:div.find-top
           (calendar (cal-start from) from to (day-counts filtered time-of-day include-sold-out?) href)
          [:form.find {:method "get" :action "/find"}
           (when like-key [:input {:type "hidden" :name "like" :value like-key}])
           ;; dates are picked on the calendar or the quick links; the form keeps them
           [:input {:type "hidden" :name "from" :value (str from)}]
           [:input {:type "hidden" :name "to" :value (str to)}]
           [:p.quick "Quick dates: "
            (for [[label f t] (quick-ranges)]
              [:a {:href (href f t)} label])]
           [:div.row
            [:label "Time "
             [:select {:name "time"}
              (for [[v l] [["any" "Any time"] ["daytime" "Daytime"] ["evening" "Evening (5pm+)"]]]
                [:option {:value v :selected (= v time-of-day)} l])]]]
           [:fieldset [:legend "Kinds (none ticked = all)"]
            (for [[k label] rank/kind-labels]
              [:label.check [:input {:type "checkbox" :name "kind" :value (name k) :checked (checked? k)}] label])]
           [:div.row
            [:label "Max price £" [:input {:type "number" :name "max_price" :min 0 :step 1 :value (p "max_price") :size 4}]]
            [:label "Venue contains " [:input {:type "text" :name "venue" :value (p "venue")}]]
            [:label.check [:input {:type "checkbox" :name "sold_out" :checked include-sold-out?}] "Include sold out"]]
           [:div.row
            [:label.grow "Looking for "
             [:input {:type "text" :name "q" :value q :placeholder "e.g. folk music, something about history, silly fun"}]]
            [:button {:type "submit"} "Find"]]]]
          (no-taste-note examples)
          [:p.count (str (count candidates) " events match "
                         (if (= from to) (fmt from "EEE d MMM") (str (fmt from "d MMM") "–" (fmt to "d MMM")))
                         ", best matches first.")]
          (when (seq rows)
            [:table.results
             [:thead [:tr [:th "When"] [:th "Event"] [:th "Venue"] [:th "Type"] [:th "Price"] [:th {:colspan 7} [:span.sr "Actions"]]]]
             rows])
          [:script (h/raw details-script)])))

;;; --- Quick rate ---------------------------------------------------------------

(defn rate-page [ds params]
  (let [{:keys [items verdicts hidden model]} (ctx ds)
        random? (= "random" (get params "mode"))
        unrated (->> items
                     (remove #(verdicts (:key %)))
                     (remove #(hidden (:key %)))
                     (filter :description)
                     (filter #(@store/vectors (:hash %))))
        sample (if random?
                 ;; a spread across kinds, shuffled within each kind; round-robin:
                 ;; one of each kind, then a second of each, …
                 (let [by-kind (vals (group-by rank/kind (shuffle unrated)))]
                   (->> (range 3)
                        (mapcat (fn [i] (keep #(nth % i nil) by-kind)))
                        (take 12)))
                 (rank/learning-batch model unrated 12))]
    (page (if random? "Quick rate" "Help me learn") "/rate"
          (if random?
            [:p.intro "A random spread of upcoming events. Rate anything you have a view on. "
             [:a {:href "/rate"} "Or help the model learn"] " with the events it's least sure about."]
            [:p.intro "The events whose ratings would teach the model most: ones it can't place between "
             "things you liked and things you didn't, and ones unlike anything you've rated yet. "
             "😐 is a useful answer. Hide only says “don't show me this”; it isn't a rating. "
             [:a {:href "/rate?mode=random"} "Random spread instead"] "."])
          (for [e (remove nil? sample)] (card e nil))
          [:p [:a.button {:href (if random? "/rate?mode=random" "/rate")} "Show me another batch"]])))

;;; --- Taste -------------------------------------------------------------------

(defn taste-page [ds _params]
  (let [fb (store/feedback ds)
        stmts (store/statements ds)
        hist (store/history ds)
        hidden (store/hidden ds)]
    (page "My taste" "/taste"
          [:section
           [:h2 "Things I like (or don't)"]
           [:p "Plain sentences work best, e.g. “I love small folk gigs”, “talks about science or local history”, “not tribute acts”."]
           [:form.row {:method "post" :action "/statements"}
            [:input.grow {:type "text" :name "text" :required true :placeholder "I like…"}]
            [:select {:name "weight"} [:option {:value "1"} "I like this"] [:option {:value "-1"} "Not for me"]]
            [:button {:type "submit"} "Add"]]
           [:ul (for [{:keys [id text weight]} stmts]
                  [:li (if (pos? weight) "👍 " "👎 ") text " "
                   [:form.inline {:method "post" :action (str "/statements/" id "/delete")} [:button "Remove"]]])]]
          [:section
           [:h2 "Things I've been to"]
           [:p "Paste past events, one per line: " [:code "Title | Venue | 2024-05-01 | notes"]
            " (only the title is needed). These count as strong likes."]
           [:form {:method "post" :action "/history"}
            [:textarea {:name "lines" :rows 6 :placeholder "Gotts Street Park | Bristol Beacon | 2025-11-28"}]
            [:p [:button {:type "submit"} "Add"]]]
           (when (seq hist)
             [:details [:summary (str (count hist) " past events")]
              [:ul (for [{:keys [id title venue date]} hist]
                     [:li (str/join " · " (remove str/blank? [date title venue])) " "
                      [:form.inline {:method "post" :action (str "/history/" id "/delete")} [:button "Remove"]]])]])]
          [:section
           [:h2 "Ratings"]
           (if (empty? fb)
             [:p "None yet."]
             (let [by-verdict (group-by :verdict fb)]
               (for [verdict (keys verdict-labels)
                     :let [rows (by-verdict verdict)]
                     :when rows]
                 [:details [:summary (str (verdict-labels verdict) " (" (count rows) ")")]
                  [:ul (for [{:keys [key title venue]} rows]
                         [:li title (when venue (str " · " venue)) " "
                          [:form.inline {:method "post" :action "/feedback/clear"}
                           [:input {:type "hidden" :name "key" :value key}]
                           [:button "Undo"]]])]])))]
          (when (seq hidden)
            [:section
             [:h2 "Hidden"]
             [:p "Events you asked not to see. Hiding isn't a rating, so these don't affect suggestions."]
             [:details [:summary (str (count hidden) " hidden")]
              [:ul (for [{:keys [key title venue]} hidden]
                     [:li title (when venue (str " · " venue)) " "
                      [:form.inline {:method "post" :action "/hidden/clear"}
                       [:input {:type "hidden" :name "key" :value key}]
                       [:button "Show again"]]])]]]))))

;;; --- actions ---------------------------------------------------------------

(defn- render-item
  "The card or row for `key` after a change, to swap in where it was."
  [ds item {:strs [view from to time]}]
  (let [v ((store/feedback-map ds) (:key item))
        item (assoc item :hidden? (contains? (store/hidden-keys ds) (:key item)))]
    (str (h/html
          (if (= "row" view)
            ;; keep the search's dates (sent back via the row's hx-vals); the score isn't recomputed
            (let [shown (filter (partial time-ok? time) (dates-in item (->date from) (->date to)))]
              (row (assoc item :window {:from from :to to :time time}) v (if (seq shown) shown (:dates item))))
            (card item v))))))

(defn- item-for [ds key] (first (filter #(= key (:key %)) (store/items ds))))

(defn feedback! [ds params]
  (let [key (get params "key")
        item (item-for ds key)]
    (store/set-feedback! ds (or item {:key key}) (get params "verdict"))
    (when item (render-item ds item params))))

(defn hide! [ds params]
  (let [key (get params "key")
        item (item-for ds key)]
    (store/set-hidden! ds (or item {:key key}) (= "true" (str (get params "hidden"))))
    (when item (render-item ds item params))))

(defn add-history-lines! [ds text]
  (doseq [line (str/split-lines (or text ""))
          :let [[title venue date notes] (map str/trim (str/split line #"\s*[|\t]\s*"))]
          :when (not (str/blank? title))]
    (store/add-history! ds {:title title
                            :venue (not-empty venue)
                            :date (not-empty date)
                            :notes (not-empty notes)})))

(defn status-fragment []
  (let [{:keys [embedded total]} @status]
    (if (and (pos? total) (< embedded total))
      (str "Reading event descriptions: " embedded " / " total "…")
      "")))
