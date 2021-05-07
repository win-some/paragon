(ns paragon.components.circle-db.specs
  (:require [clojure.spec.alpha :as s]
            [ghostwheel.core :as gw :refer [>defn =>]]))


(s/def ::cardinality #{:db/single :db/multiple})
(s/def ::type keyword?)
(s/def ::ref #(= :db/ref (:type (meta %))))
(s/def ::attr (s/and (s/keys :req [::name ::value ::ts ::prev-ts])
                     #(s/valid? ::cardinality (::cardinality (meta %)))
                     #(s/valid? ::type (::type (meta %)))))
(s/def ::attrs (s/coll-of ::attr))
(s/def ::id (s/or :temp #{:db/no-id-yet}
                  :real int?))
(s/def ::entity (s/keys :req [::id ::attrs]))

(s/def ::storage (s/map-of ::id ::entity))
(s/def ::from-eav fn?)
(s/def ::to-eav fn?)
(s/def ::usage-pred fn?)
(s/def ::index (s/and map?
                      #(s/valid? (s/keys :req [::from-eav ::to-eav ::usage-pred]) (meta %))))
(s/def ::VAET ::index)
(s/def ::AVET ::index)
(s/def ::VEAT ::index)
(s/def ::EAVT ::index)
(s/def ::layer (s/keys :req [::storage ::VAET ::AVET ::VEAT ::EAVT]))
(s/def ::layers (s/coll-of ::layer))
(s/def ::top-id int?)
(s/def ::curr-time int?)
(s/def ::database (s/keys :req [::layers ::top-id ::curr-time]))

(>defn ref?
  [attr]
  [::attr => boolean?]
  (= :db/ref (:type (meta attr))))

(comment
  (s/valid? ::database
            {::top-id 0
             ::curr-time 0
             ::layers [{::storage {}
                        ::VAET (with-meta {} {::from-eav   (fn [e a v] [v a e])
                                              ::to-eav     (fn [v a e] [e a v])
                                              ::usage-pred ref?})
                        ::AVET (with-meta {} {::from-eav   (fn [e a v] [a v e])
                                              ::to-eav     (fn [a v e] [e a v])
                                              ::usage-pred (constantly true)})
                        ::VEAT (with-meta {} {::from-eav   (fn [e a v] [v e a])
                                              ::to-eav     (fn [v e a] [e a v])
                                              ::usage-pred (constantly true)})
                        ::EAVT (with-meta {} {::from-eav   (fn [e a v] [e a v])
                                              ::to-eav     (fn [e a v] [e a v])
                                              ::usage-pred (constantly true)})}]})
  true


  )
