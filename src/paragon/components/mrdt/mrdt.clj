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
         ob #mset []]
    (if head
      (recur rest (into ob (for [x rest] [head x])))
      ob)))

(defn build-kv
  "Build the key-value relation for a map."
  [m]
  (set m))

(defn uniquify-seq
  "Pair each element with its count in the sequence. This makes it easy to build a
  directed graph of the nodes without having to worry about repeated values. It also
  nicely solves the problem of having falsey values.

  ex. [1 2 1] -> [[1 1] [2 1] [1 2]]"
  [s]
  (->> s
       (reduce (fn [res x]
                 (if-let [x-count (get (:freqs res) x)]
                   (-> res
                       (update-in [:freqs x] inc)
                       (update :res conj [x (inc x-count)]))
                   (-> res
                       (assoc-in [:freqs x] 1)
                       (update :res conj [x 1]))))
               {:freqs {} :res []})
       :res))

(defn seq->relations
  [s]
  (let [uniquified (uniquify-seq s)]
    {:mem (build-mem uniquified)
     :ord (build-ob uniquified)}))

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

(defn get-path
  [g src dest]
  (loop [curr src
         to-visit []
         path []]
    (let [node (get g curr)
          [next-curr & next-to-visit] (concat (:out node) to-visit)]
      (cond (not next-curr)
            (when-not (empty? path)
              (conj path curr))
            (= curr dest)
            (conj path curr)
            (not (:out node))
            (recur next-curr next-to-visit [src])
            :else
            (recur next-curr next-to-visit (conj path curr))))))

(defn add-edge
  [g [src dest]]
  (-> g
      (assoc-in [src :val] src)
      (assoc-in [dest :val] dest)
      (update-in [src :out] (fnil conj #{}) dest)
      (update-in [dest :in] (fnil conj #{}) src)))

(defn build-graph
  [edges]
  (reduce add-edge {} edges))

(defn remove-edge
  [g [src dest]]
  (-> g
      (update-in [src :out] disj dest)
      (update-in [dest :in] disj src)))

(defn find-incoming-edges [g node]
  (map #(vector % node) (get-in g [node :in])))

(defn bfs
  "Find the path from start to target, if it exists. Not stack safe."
  ([g target start]
   (bfs g target [] start))
  ([g target path curr]
   (let [{children :out} (get g curr)
         next-path (conj path curr)]
     (cond (= target curr)
           next-path

           children
           (not-empty (mapcat (partial bfs g target next-path)
                              children))
           :else
           []))))


(comment
  (bfs b 3 1)
  (1 2 3)
  nil
  ()
  [1]
  (1 5 6)
  (2 3 4)
  (1 2 3 4)
  (1 2 3)
  (1 2 3)
  [[[[] nil] nil]]
  []
  [[]]

  )

(defn prune
  "Remove edge (u,v) if there exists edges (u, w) and (w, v)."
  [g]
  (->> (keys g)
       ;; find all the edges that link to a node
       (map (partial find-incoming-edges g))
       ;; if there's more than one link
       (filter #(>= (count %) 2))
       (reduce (fn [reduced-g incoming-edges]
                 (reduce (fn [reducing-g [e1 e2]]
                           ;; if there's a path from edge 1 to edge 2
                           (cond (bfs reducing-g (first e2) (first e1))
                                 ;; remove it
                                 (remove-edge reducing-g e1)
                                 ;; if there's path from edge 2 to edge 1
                                 (bfs reducing-g (first e1) (first e2))
                                 ;; remove it
                                 (remove-edge reducing-g e2)
                                 ;; there should always be a path between the two, this is just for safety
                                 :else
                                 reducing-g))
                         reduced-g
                         (partition 2 1 incoming-edges))) ; break edges into pairs
               g)))

(comment
  (def edges [[1 2] [2 3] [3 4] [1 4] [1 3] [2 4]])
  (def a (reduce add-edge {} edges))
  (def edges2 [[1 2] [2 3] [3 4] [1 5] [5 6] [6 7]])
  (def b (build-graph edges2))
  (def edges3 [[1 2] [1 3]])
  (def c (build-graph edges3))
  (def d (build-graph [[1 2] [1 3] [3 5] [2 4]]))
  b
  (graph/pprint (apply graph/multidigraph edges))
  (graph/pprint (transitively-reduce-graph (apply graph/multidigraph edges)))
  (prune d)
  {1 {:val 1,           :out #{3 2}},
   2 {:val 2, :in #{1}, :out #{4}},
   3 {:val 3, :in #{1}, :out #{5}},
   5 {:val 5, :in #{3}},
   4 {:val 4, :in #{2}}}

  {1 {:val 1, :out #{3 2}},
   2 {:val 2, :in #{1}},
   3 {:val 3, :in #{1}}}

  {1 {:val 1,           :out #{2}},
   2 {:val 2, :in #{1}, :out #{3}},
   3 {:val 3, :in #{2}, :out #{4}},
   4 {:val 4, :in #{3}}}


  (1 2 3 4)

  (bfs a 1 3)
  [1 4 3]
  nil
  [3 4]
  [3 4 nil]
  [1 4 3]
  [1 4]

  [1 nil nil nil]
  (prune a)
  (([1 3] [2 3]) ([1 4] [3 4] [2 4]))
  (() ([1 2]) ([1 3] [2 3]) ([1 4] [3 4] [2 4]))
  (1 2 3 4)


  (-> {}
      (add-edge [1 2])
      (add-edge [2 3])
      (add-edge [3 4])
      (add-edge [1 4]))
  {1 {:val 1, :out #{4 2}},
   2 {:val 2, :in #{1}, :out #{3}},
   3 {:val 3, :in #{2}, :out #{4}},
   4 {:val 4, :in #{1 3}}}

  (get-path a 1 3)
  [1 2 3]
  (get-path a 1 4)
  [1 4]
  (get-path a 4 1) ;; should be nil or empty
  nil
  [4]

  [1 2 3]
  [2 3 4]

  [1 2 3]
  [1 2 3 4]
  [1 4 2 3]
  [1 4 2 3]

  [1 2]
  [1 2 3]
  [1 1 2]

  )

(defn topo-sort
  "Topographically sort a tree, using the ordering function to provide a consistent total
  order in cases where a node has multiple outgoing edges. Not stack safe. Not type safe."
  ([g]
   ;; this will blow up on non-number collections
   (topo-sort g <))
  ([g ord]
   (topo-sort (prune g) ord (->> (vals g)
                                 (filter (complement :in))
                                 (first)
                                 (:val))))
  ([g ord start]
   (loop [node (get g start)
          res [start]]
     (let [[next & rst :as children] (:out node)]
       (cond (not next)
             res
             rst
             (concat res (mapcat (partial topo-sort g ord) (sort-by identity ord children)))
             :else
             (recur (get g next) (conj res next)))))))

(comment
  b
  {1 {:val 1,           :out #{2 5}},
   2 {:val 2, :in #{1}, :out #{3}},
   3 {:val 3, :in #{2}, :out #{4}},
   4 {:val 4, :in #{3}},
   5 {:val 5, :in #{1}, :out #{6}},
   6 {:val 6, :in #{5}, :out #{7}},
   7 {:val 7, :in #{6}}}
  (topo-sort c <)
  (1 2 3)
  (1 3 2)
  (1 3 5 2 4)
  (1 2 4 3 5)
  (1 2 3)
  (1 2 3 4 5 6 7)
  [1 2 3 4]

  (let [[next & rst :as children] #{1 2 3}]
    [next rst children])
  [1 (3 2) #{1 3 2}]

  [1 2 3 4]


  ({:val 1, :out #{4 3 2}}))


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
  [{:keys [mem ord]}]
  ;; step 1 - build directed graph
  ;; step 2 - reduce graph
  ;; step 3 - sort it topologically
  ;; step 4 - apply arbitration edge for consistent order
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
