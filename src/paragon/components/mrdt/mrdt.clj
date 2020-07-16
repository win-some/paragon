(ns paragon.components.mrdt.mrdt
  (:require [idle.multiset.api :as mset]
            [clojure.set :as set]
            [ubergraph.core :as graph]
            [ubergraph.alg :as alg]
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
         ob (mset/multiset)]
    (if head
      (recur rest (into ob (for [x rest] [head x])))
      ob)))

(defn build-kv
  "Build the key-value relation for a map."
  [m]
  (set m))

(defn seq->relations
  [s]
  {:mem (build-mem s)
   :ord (build-ob s)})

(defn transitively-reduce-graph
  "Remove edge (u,v) if there exists edges (u, w) and (w, v)."
  [g]
  (->> (graph/nodes g)
       ;; find all the edges that link to a node
       (map #(graph/find-edges g {:dest %}))
       ;; if there's more than one link
       (filter #(>= (count %) 2))

       (reduce (fn [new-g edges-that-link-to-node]
                 (reduce (fn [new-g' [e1 e2]]
                           ;; if there's a path from edge 1 to edge 2
                           (cond (alg/shortest-path new-g' (graph/src e1) (graph/src e2))
                                 ;; remove it
                                 (graph/remove-edges* new-g' [e1])
                                 ;; path from edge 2 to edge 1
                                 (alg/shortest-path new-g' (graph/src e2) (graph/src e1))
                                 ;; remove it
                                 (graph/remove-edges* new-g' [e2])
                                 ;; there should always be a path between the two, this is just for safety
                                 :else
                                 new-g'))
                         new-g
                         (partition 2 1 edges-that-link-to-node))) ; break edges into pairs
               g)))

(defn relations->seq
  [{:keys [mem ord]}]
  ;; step 1 - build directed graph
  ;; step 2 - sort it topologically
  ;; step 3 - apply arbitration edge for consistent order
  ;; right now I'm just crudely sorting the ord coll to handle step 3
  ;; ideally topsort should take a comparator function to break ties
  ;; and apply a total order to the result
  ;; URG topsort doesn't work with multigraphs
  (cond
    (empty? mem) '()
    (empty? ord) (seq mem)
    :else
    (->> ord
         (apply graph/multidigraph)
         (transitively-reduce-graph)
         (extra/topsort2)
         (:topological-order))))

(comment
  (seq->relations [1 2 3])
  {:mem [1 2 3], :ord [[1 2] [1 3] [2 3]]}
  (relations->seq (seq->relations [1 2 3]))
  '(1 2 3)

  (relations->seq (seq->relations [1 2 3 4 5]))
  '(1 2 3 4 5)
  (relations->seq (seq->relations [1 2 false nil  true]))
  ;; doesen't work with nil or false

  (relations->seq {:mem [1 2 3] :ord #mset [[1 2] [1 3]]})
  [1 3 2]
  {:has-cycle? false, :topological-order [1 3 2]}
  '(1 3 2)

  (reduce str (relations->seq (seq->relations "helo!")))
  "helo!"


  (seq->relations [1 1 1])
  {:mem #mset [1 1 1], :ord #mset [[1 1] [1 1] [1 1]]}
  (relations->seq {:mem #mset [1 1 1], :ord #mset [[1 1] [1 1] [1 1]]})
  {:has-cycle? true}
  ;; broken with multigraphs :(




  )

(defprotocol Mergeable
  (abstractify [this])
  (three-way-merge [r1 r2 lca]))

(extend-type clojure.lang.Sequential
  Mergeable
  (abstractify [s] (seq->relations s))
  (three-way-merge [r1 r2 lca]
         (let [{lca-mem :mem lca-ord :ord} (abstractify lca)
               {r1-mem :mem r1-ord :ord} (abstractify r1)
               {r2-mem :mem r2-ord :ord} (abstractify r2)
               merged-mem (mset/union (mset/intersection lca-mem r1-mem r2-mem)
                                      (mset/difference r1-mem lca-mem)
                                      (mset/difference r2-mem lca-mem))]
           (relations->seq {:mem merged-mem
                            :ord (mset/intersection (mset/union (mset/intersection lca-ord r1-ord r2-ord)
                                                                (mset/difference r1-ord lca-ord)
                                                                (mset/difference r2-ord lca-ord))
                                                    ;; the new order can only contain merged members
                                                    (mset/product merged-mem merged-mem))}))))

(extend-type java.lang.String
  Mergeable
  (abstractify [s] (seq->relations s))
  (three-way-merge [r1 r2 lca]
    (let [{lca-mem :mem lca-ord :ord} (abstractify lca)
          {r1-mem :mem r1-ord :ord} (abstractify r1)
          {r2-mem :mem r2-ord :ord} (abstractify r2)
          merged-mem (mset/union (mset/intersection lca-mem r1-mem r2-mem)
                                 (mset/difference r1-mem lca-mem)
                                 (mset/difference r2-mem lca-mem))]
      (reduce str (relations->seq {:mem merged-mem
                                   :ord (mset/intersection (mset/union (mset/intersection lca-ord r1-ord r2-ord)
                                                                       (mset/difference r1-ord lca-ord)
                                                                       (mset/difference r2-ord lca-ord))
                                                           ;; the new order can only contain merged members
                                                           (mset/product merged-mem merged-mem))})))))

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
  (three-way-merge [1 3] [1 2] [1] )
  [1 3 2]
  (three-way-merge '(1 3) '(1 2) '(1) )
  [1 2 3]
  (three-way-merge "helo wurd!" "helo!:D" "helo")
  "helo wurd!:D"
  (three-way-merge {:a 1} {:b 2} {:c 2})
  {:b 2, :a 1}
  (three-way-merge #{1 2 3} #{4 5 6} #{1 10})
  #{4 6 3 2 5}

  )
