(ns paragon.components.mrdt.mrdt
  (:require [idle.multiset.api :as mset]
            [ubergraph.core :as graph]
            [ubergraph.alg :as alg]
            [paragon.components.ubergraph-extras :as extra]))

(defn build-mem
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

(defn seq->relations
  [s]
  {:mem (build-mem s)
   :ord (build-ob s)})

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
    :else (extra/topsort2 (apply graph/multidigraph (sort ord)))))

(comment
  (seq->relations [1 2 3])
  {:mem [1 2 3], :ord [[1 2] [1 3] [2 3]]}
  (relations->seq (seq->relations [1 2 3]))
  {:has-cycle? false, :topological-order [1 2 3]}


  (seq->relations [1 1 1])
  {:mem #mset [1 1 1], :ord #mset [[1 1] [1 1] [1 1]]}
  (relations->seq {:mem #mset [1 1 1], :ord #mset [[1 1] [1 1] [1 1]]})
  {:has-cycle? true}
  ;; broken with multigraphs :(
  nil
  nil

  (seq->relations [1 2 1])
  {:mem #mset [1 1 2], :ord #mset [[1 2] [1 1] [2 1]]}
  {:mem [1 1 1], :ord [[1 1] [1 1] [1 1]]}
  (relations->seq (seq->relations [1 1 1]))

  (extra/topsort2 (graph/multidigraph [1 2] [1 3]))
  {:has-cycle? false, :topological-order [1 3 2]}

  (graph/pprint (graph/multidigraph [1 1] [1 1]))
  (extra/topsort2 (graph/multidigraph [1 1] [1 1]))
  {:has-cycle? true}
  (alg/topsort (graph/multidigraph [1 1] [1 1]))
  ;; an edge from 1 to 1 has to be two different ids
  nil
  (extra/topsort2 (graph/multidigraph [1 2] [1 1] [2 1]))
  nil
  (graph/pprint (graph/multidigraph [1 2] [1 1] [2 1]))
  nil


  )

(defn merge-replicas
  "Take the characteristic relations of the lowest common ancestor (lca), replica 1 (r1),
  and replica 2 (r2) and merge them."
  [lca r1 r2]
  (let [{lca-mem :mem lca-ord :ord} lca
        {r1-mem :mem r1-ord :ord} r1
        {r2-mem :mem r2-ord :ord} r2
        merged-mem (mset/union (mset/intersection lca-mem r1-mem r2-mem)
                               (mset/difference r1-mem lca-mem)
                               (mset/difference r2-mem lca-mem))]
    {:mem merged-mem
     :ord (mset/intersection (mset/union (mset/intersection lca-ord r1-ord r2-ord)
                                         (mset/difference r1-ord lca-ord)
                                         (mset/difference r2-ord lca-ord))
                             ;; the new order can only contain merged members
                             (mset/product merged-mem merged-mem))}))



(comment

  (merge-replicas (seq->relations [1 1 1])
                  (seq->relations [1 2 3])
                  (seq->relations [4 5 6]))


  {:mem [2 3 4 5 6], :ord [[2 3] [4 5] [4 6] [5 6]]}

  {:mem [2 3 4 5 6], :ord [[1 2] [1 3] [2 3] [4 5] [4 6] [5 6]]})

(comment
  (let [l (seq->relations [1 2])
        r1 (seq->relations [2])
        r2 (seq->relations [2])]
    (relations->seq (merge-replicas l r2 r1))
    (relations->seq (merge-replicas l r1 r2))
    )
  '(2)
  '(2)

  ;; ---
  (let [l (seq->relations [1])
        r1 (seq->relations [1 2])
        r2 (seq->relations [1 3])]
    (relations->seq (merge-replicas l r2 r1))
    (relations->seq (merge-replicas l r1 r2)))
  '(1 3 2)
  '(1 3 2)
  ;; expected ord [[1 2] [1 3] [2 3]] -> where could the [2 3] come from?
  ;; the arbitration set

  ;; --
  (let [l (seq->relations [1])
        r1 (seq->relations [])
        r2 (seq->relations [])
        r2' (seq->relations [2])]
    (relations->seq (merge-replicas l r1 r2))
    (relations->seq (merge-replicas l r1 r2'))
    (relations->seq (merge-replicas l r2' r1)))
  '(2)
  '(2)
  '()
  {:mem [2], :ord []}
  {:mem [2], :ord []}
  {:mem [], :ord []}



  (let [l (seq->relations "Hello!")
        r1 (seq->relations "Hello world!")
        r2 (seq->relations "Hello! :D")]
    (relations->seq (merge-replicas l r1 r2))
    #_(relations->seq (merge-replicas l r2 r1)))
  ;; this doesn't work yet
  nil



  )
