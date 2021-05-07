(ns paragon.components.mrdt.mrdt
  (:require [idle.multiset.api :as mset]
            [clojure.set :as set]
            [paragon.components.mrdt.graph :as graph]
            [paragon.components.ubergraph-extras :as extra]))

(defn build-mem
  "Returns a multiset of the collection's members."
  [coll]
  (mset/multiset coll))

(defn build-ob
  "Build the observed-before (ob) relation relating every pair of elements x, y in coll such
  that x occurs before y."
  [coll]
  (loop [[head & rest] coll
         ob #mset []]
    (if head
      (recur rest (into ob (for [x rest] [head x])))
      ob)))

(defn build-kv
  "Build the key-value relation for a map."
  [m]
  (set m))

(defn seq->relations
  [s]
  (let [val-ids (map (fn [x] (java.util.UUID/randomUUID)) s)]
    {:mem (build-mem val-ids)
     :ord (build-ob val-ids)
     ;; TODO: you can't map the values at abstraction time, you need to store that with
     ;; the the actual sequence
     :val-map (zipmap val-ids s)}))

(comment
  [1 2 1] -> [[1 1] [2 1] [1 2]]

  (reduce (fn [res x]
            (if-let [x-count (get (:freqs res) x)]
              (-> res
                  (update-in [:freqs x] inc)
                  (update :res conj [x (inc x-count)]))
              (-> res
                  (assoc-in [:freqs x] 1)
                  (update :res conj [x 1]))))
          {:freqs {} :res []}
          [1 2 1])
  {:freqs {1 2, 2 1}, :res [[1 1] [2 1] [1 2]]}

  )


;; a -> b
;; b -> c
;; a -> c (delete this one)

;; [1 2 1]
;; [uuid1 uuid2 uuid3]
;; [1 2 3]
;; [[1 1] [2 1] [1 2]]
;; membership relation
;; order relation
;; (map first)

(defn relations->seq
  [{:keys [mem ord val-map]}]
  (cond
    (empty? mem) '()
    (empty? ord) (seq mem)
    :else
    (->> ord
         (graph/build-graph)
         (graph/topo-sort)
         (map val-map))))

(comment
  (seq->relations [1 2 3])
  {:mem [1 2 3], :ord [[1 2] [1 3] [2 3]]}
  (relations->seq (seq->relations [1 2 3]))
  '(1 2 3)

  (->> [[1 2] [1 3] [2 3]]
       (build-graph))

  (relations->seq (seq->relations [1 2 3 4 5]))

  '(1 2 3 4 5)
  '(1 2 3 4 5)
  (relations->seq (seq->relations [1 2 false nil  true]))
  '(1 2 false nil true)

  (relations->seq (seq->relations [1 1 1]))
  '(1 1 1)

  (relations->seq (seq->relations [1 2 1]))
  (1 2 1)
  )

(defprotocol Mergeable
  (abstractify [this])
  (three-way-merge [r1 r2 lca]))

(defn product
  [s1 s2]
  (set (for [a s1 b s2]
         [a b])))

(defn three-way-merge-sequence
  [r1 r2 lca]
  (let [{lca-mem :mem lca-ord :ord :as lca-a} (abstractify lca)
        {r1-mem :mem r1-ord :ord :as r1-a} (abstractify r1)
        {r2-mem :mem r2-ord :ord :as r2-a} (abstractify r2)
        merged-mem (mset/union (mset/intersection lca-mem r1-mem r2-mem)
                               (mset/difference r1-mem lca-mem)
                               (mset/difference r2-mem lca-mem))]
    (relations->seq {:mem merged-mem
                     :ord (mset/intersection (mset/union (mset/intersection lca-ord r1-ord r2-ord)
                                                         (mset/difference r1-ord lca-ord)
                                                         (mset/difference r2-ord lca-ord))
                                             ;; the new order can only contain merged members
                                             (mset/product merged-mem merged-mem))
                     :val-map (merge (:val-map lca-a) (:val-map r1-a) (:val-map r2-a))})))

(extend-type clojure.lang.Sequential
  Mergeable
  (abstractify [s] (seq->relations s))
  (three-way-merge [r1 r2 lca]
    (three-way-merge-sequence r1 r2 lca)))

(defn three-way-merge-string
  [r1 r2 lca]
  (let [{lca-mem :mem lca-ord :ord} (abstractify lca)
        {r1-mem :mem r1-ord :ord} (abstractify r1)
        {r2-mem :mem r2-ord :ord} (abstractify r2)
        merged-mem (mset/union (mset/intersection lca-mem r1-mem r2-mem)
                               (mset/difference r1-mem lca-mem)
                               (mset/difference r2-mem lca-mem))]
    {:mem merged-mem
     :ord (set/intersection (set/union (set/intersection lca-ord r1-ord r2-ord)
                                       (set/difference r1-ord lca-ord)
                                       (set/difference r2-ord lca-ord))
                            ;; the new order can only contain merged members
                            (product merged-mem merged-mem))}))

(extend-type java.lang.String
  Mergeable
  (abstractify [s] (seq->relations s))
  (three-way-merge [r1 r2 lca]
    ;; delegate to Sequential implementation
    (apply str (three-way-merge (seq r1) (seq r2) (seq lca)))))

(extend-type clojure.lang.IPersistentMap
  Mergeable
  (abstractify [m] {:kv (build-kv m)})
  (three-way-merge [r1 r2 lca]
    (let [lca-kv (:kv (abstractify lca))
          r1-kv  (:kv (abstractify r1))
          r2-kv  (:kv (abstractify r2))
          merged (set/union (set/intersection lca-kv r1-kv r2-kv)
                                (set/difference r1-kv lca-kv)
                                (set/difference r2-kv lca-kv))]
      (into {} merged))))

(extend-type clojure.lang.IPersistentSet
  Mergeable
  (abstractify [s] {:mem s})
  (three-way-merge [r1 r2 lca]
    (let [lca-mem (:mem (abstractify lca))
          r1-mem  (:mem (abstractify r1))
          r2-mem  (:mem (abstractify r2))]
      (set/union (set/intersection lca-mem r1-mem r2-mem)
                 (set/difference r1-mem lca-mem)
                 (set/difference r2-mem lca-mem)))))


(comment
  ;;
  (three-way-merge [1 3] [1 2] [1] )

  [1 3 2]
  (three-way-merge '(1 3) '(1 2) '(1) )
  [1 2 3]
  (three-way-merge "helo wurd!" "helo!:D" "helo")

  "helo wurd!:D"
  (three-way-merge "hello world!" "hello! :D" "hello!")
  ""
  (def rs (three-way-merge-string "h u!" "h! :D" "h!"))
  #{[[\space 1] [\u 1]]
    [[\u 1] [\! 1]]
    [[\space 1] [\! 1]]
    [[\h 1] [\u 1]]
    [[\h 1] [\space 1]]
    [[\h 1] [\! 1]]}
  #{[[\! 1] [\: 1]]
    [[\! 1] [\space 1]]                 ; here
    [[\space 1] [\: 1]]
    [[\h 1] [\space 1]]
    [[\h 1] [\D 1]]
    [[\h 1] [\: 1]]
    [[\! 1] [\D 1]]
    [[\space 1] [\D 1]]
    [[\h 1] [\! 1]]
    [[\: 1] [\D 1]]}
  #{[[\h 1] [\! 1]]}
  (def ord (seq (:ord rs)))

  (def g (relations->seq rs))
  ;; dag? false

  (reduce #(let [next-g (graph/add-edges %1 %2)]
             (if (alg/dag? next-g)
               (reduced [%2 %1])
               next-g))
          (graph/digraph)
          (:ord g))


  (three-way-merge [2] [2] [])
  '(2 2)

  (three-way-merge {:a 1} {:b 2} {:c 2})
  {:b 2, :a 1}
  (three-way-merge #{1 2 3} #{4 5 6} #{1 10})
  #{4 6 3 2 5})
