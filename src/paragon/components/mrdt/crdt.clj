(ns paragon.components.mrdt.crdt
  (:require [clojure.set :as set]))

(defn add-set1
  [s x]
  (conj s x))

(comment
  ;; append-only sets are CRDTs out of the box
  (add-set1 #{} 1)
  #{1}

  )

(defn add-set2
  [s x]
  (update s :added conj x))

(defn remove-set2
  [s x]
  (-> s
      (update :added disj x)
      (update :removed conj x)))

(defn read-set2
  [s]
  (set/difference (:added s) (:removed s)))

(comment
  ;; but what if we want to remove things?
  ;; well, we can add another set to keep track of removals
  (read-set2 (add-set2 {:added #{} :removed #{}} 1))
  #{1}

  (-> {:added #{} :removed #{}}
      (add-set2 1)
      (remove-set2 1)
      (read-set2))

  ;; but we can only remove once ever! because we can't guarantee what order the operations happen in
  (-> {:added #{} :removed #{}}
      (add-set2 1)
      (remove-set2 1)
      (add-set2 1)
      (read-set2))
  #{}


  )

(defn add-set3
  [s x]
  (-> s
      (update-in [:added x] (fnil inc 0))))

(defn remove-set3
  [s x]
  (-> s
      (update-in [:removed x] (fnil inc 0))))

(defn read-set3
  [s]
  (let [merged-freqs (reduce (fn [res [removed-k removed-count]]
                               (update res removed-k (fnil - 0) removed-count))
                             (:added s)
                             (:removed s))]
    (->> merged-freqs
         (filter (fn [[k v]] (pos? v)))
         (map first)
         (into #{}))))

(comment
  ;; so for a proper set, we need more information
  ;; remember counters, the simplest crdt? we'll use those
  (-> {:added {} :removed {}}
      (add-set3 1)
      (remove-set3 1)
      (add-set3 1)
      (read-set3))
  #{1}

  (disj #{} 1)
  #{}

  "hello"
  "hello world"
  "yo"


  )
