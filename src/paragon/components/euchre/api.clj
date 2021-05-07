(ns paragon.components.euchre.api
  (:require [malli.core :as m]
            [clojure.spec.alpha :as s]
            [ghostwheel.core :as g :refer [>defn]]))

(s/def ::rank (s/int-in 9 15))
(s/def ::suit #{:clubs :diamonds :hearts :spades})
(s/def ::card (s/cat :rank ::rank :suit ::suit))

(def ranks
  "nine ten jack queen king ace"
  (range 9 15))

(def suits
  [:clubs :diamonds :hearts :spades])

(def deck
  (for [suit suits rank ranks]
    [rank suit]))

(s/def ::state
  (s/keys :req [::dealer ::curr-player ::trump ::deck]))



(comment

  )
