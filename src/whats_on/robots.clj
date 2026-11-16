(ns whats-on.robots
  "Minimal robots.txt support: groups, Allow/Disallow with * and $ wildcards,
  longest-match-wins, and Crawl-delay."
  (:require [clojure.string :as str]))

(defn parse
  "Parse robots.txt text into a seq of groups {:agents #{..} :rules [..] :crawl-delay n}."
  [txt]
  (let [lines (->> (str/split-lines (or txt ""))
                   (map #(str/trim (str/replace % #"#.*" "")))
                   (remove str/blank?)
                   (keep #(when-let [[_ k v] (re-matches #"(?i)([a-z-]+)\s*:\s*(.*)" %)]
                            [(str/lower-case k) (str/trim v)])))]
    (loop [[[k v] & more :as ls] lines
           groups []
           current nil
           in-rules? false]
      (if (empty? ls)
        (cond-> groups current (conj current))
        (case k
          "user-agent" (if (and current (not in-rules?))
                         (recur more groups (update current :agents conj (str/lower-case v)) false)
                         (recur more (cond-> groups current (conj current))
                                {:agents #{(str/lower-case v)} :rules []} false))
          ("allow" "disallow") (recur more groups
                                      (cond-> current
                                        (and current (not (str/blank? v)))
                                        (update :rules conj {:allow? (= k "allow") :path v}))
                                      true)
          "crawl-delay" (recur more groups
                               (cond-> current
                                 (and current (parse-double v)) (assoc :crawl-delay (parse-double v)))
                               true)
          (recur more groups current in-rules?))))))

(defn- pattern->regex [p]
  (let [anchored? (str/ends-with? p "$")
        p (cond-> p anchored? (subs 0 (dec (count p))))]
    (re-pattern (str "^"
                     (->> (str/split p #"\*" -1)
                          (map #(java.util.regex.Pattern/quote %))
                          (str/join ".*"))
                     (when anchored? "$")))))

(defn rules-for
  "The merged group that applies to `agent` (a product token), falling back to *."
  [groups agent]
  (let [agent (str/lower-case agent)
        specific (filter #(some (fn [a] (and (not= a "*") (str/includes? agent a))) (:agents %)) groups)
        chosen (if (seq specific) specific (filter #((:agents %) "*") groups))]
    {:rules (mapcat :rules chosen)
     :crawl-delay (some :crawl-delay chosen)}))

(defn allowed?
  "Is `path` (path + query) allowed by `rules`? Longest matching rule wins; ties go to Allow."
  [{:keys [rules]} path]
  (let [matches (filter #(re-find (pattern->regex (:path %)) path) rules)]
    (if (empty? matches)
      true
      (:allow? (last (sort-by (juxt #(count (:path %)) :allow?) matches))))))
