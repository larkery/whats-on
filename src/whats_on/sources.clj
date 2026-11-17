(ns whats-on.sources
  "The registry of sources. Shared by the app's refresh (JVM) and `bb sources`."
  (:require [whats-on.sources.alma :as alma]
            [whats-on.sources.arnolfini :as arnolfini]
            [whats-on.sources.arnos-vale :as arnos-vale]
            [whats-on.sources.beacon :as beacon]
            [whats-on.sources.bristol-cathedral :as bristol-cathedral]
            [whats-on.sources.bristol-museums :as bristol-museums]
            [whats-on.sources.cube :as cube]
            [whats-on.sources.headfirst :as headfirst]
            [whats-on.sources.hen-and-chicken :as hen-and-chicken]
            [whats-on.sources.hippodrome :as hippodrome]
            [whats-on.sources.lost-in-the-reads :as lost-in-the-reads]
            [whats-on.sources.megascreen :as megascreen]
            [whats-on.sources.old-vic :as old-vic]
            [whats-on.sources.plough :as plough]
            [whats-on.sources.redgrave :as redgrave]
            [whats-on.sources.st-georges :as st-georges]
            [whats-on.sources.st-mary-redcliffe :as st-mary-redcliffe]
            [whats-on.sources.storysmith :as storysmith]
            [whats-on.sources.tobacco-factory :as tobacco-factory]
            [whats-on.sources.trinity :as trinity]
            [whats-on.sources.wardrobe :as wardrobe]
            [whats-on.sources.watershed :as watershed]
            [whats-on.sources.we-the-curious :as we-the-curious]))

;;; Each source: a fetch fn, and a pattern matching how *other* sites (i.e.
;;; Headfirst) write its venue name, used for de-duplication. Patterns are
;;; matched against lower-cased names with '&' spelled 'and' and apostrophes
;;; removed. Lower priority numbers win when the same event is listed twice.
(def registry
  {:watershed         {:fn watershed/events         :venue #"watershed"}
   :tobacco-factory   {:fn tobacco-factory/events   :venue #"tobacco factory"}
   :old-vic           {:fn old-vic/events           :venue #"old vic"}
   :hippodrome        {:fn hippodrome/events        :venue #"hippodrome"}
   :alma              {:fn alma/events              :venue #"\balma\b"}
   :wardrobe          {:fn wardrobe/events          :venue #"wardrobe"}
   :st-georges        {:fn st-georges/events        :venue #"st\.? georges? bristol|^st\.? georges?$"}
   :trinity           {:fn trinity/events           :venue #"trinity cent"}
   :beacon            {:fn beacon/events            :venue #"beacon|colston hall"}
   :cube              {:fn cube/events              :venue #"\bcube\b"}
   :redgrave          {:fn redgrave/events          :venue #"redgrave"}
   :arnolfini         {:fn arnolfini/events         :venue #"arnolfini"}
   :bristol-museums   {:fn bristol-museums/events   :venue #"m shed|bristol museum|blaise castle|georgian house|red lodge"}
   :arnos-vale        {:fn arnos-vale/events        :venue #"arnos vale"}
   :we-the-curious    {:fn we-the-curious/events    :venue #"we the curious"}
   :bristol-cathedral {:fn bristol-cathedral/events :venue #"bristol cathedral"}
   :st-mary-redcliffe {:fn st-mary-redcliffe/events :venue #"st\.? mary redcliffe"}
   :hen-and-chicken   {:fn hen-and-chicken/events   :venue #"hen\s+and\s+chicken"}
   :storysmith        {:fn storysmith/events        :venue #"storysmith"}
   :lost-in-the-reads {:fn lost-in-the-reads/events :venue #"lost in the reads"}
   :megascreen        {:fn megascreen/events        :venue #"megascreen"}
   ;; Headfirst calls it "The Plough Inn"
   :plough            {:fn plough/events            :venue #"\bplough\b"}
   :headfirst         {:fn headfirst/events         :priority 20}})
