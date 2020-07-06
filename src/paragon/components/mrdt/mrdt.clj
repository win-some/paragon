(ns paragon.components.mrdt.mrdt)

(defn seq-disj
  "Given an element and collection, will return collection without the element or the
  collection unchanged if the element is not present. Like `disj` for sequences."
  [coll x]
  (let [[first [target & rest]] (split-with (partial not= x) coll)]
    (concat first rest)))

(comment
  (seq-disj [1 2 3 3 3 4 5 7] 3)
  '(1 2 3 3 4 5 7)

  (seq-disj [1 2 3 4 nil 5] nil)
  nil

  (seq-disj [false false nil true false] true)
  '(false false nil false)

  (seq-disj [false false nil true false] false)
  '(false nil true false)

  (seq-disj '( 4 6 false ) false)
  '(4 6)
  nil
  (= false false)

  )

(defn seq-intersection
  "Returns the items that are shared between sequence 1 and sequence 2."
  ([seq1 seq2]
   ;; for each element of seq1, check if has an equal in seq2
   ;; if it does, remove that item from both seqs and put it in shared
   ;; if it does not, do nothing
   ;; at the end seq1 will be empty and seq2 will only have members that
   ;; aren't shared with seq1
   (if (< (count seq2) (count seq1))
     (recur seq2 seq1)
     (loop [[item & rest] seq1
            remaining seq2
            shared []]
       (let [new-remaining (seq-disj remaining item)]
         (cond (and (nil? item)
                    (not (seq rest)))
               shared

               (not= (count remaining) (count new-remaining))
               (recur rest new-remaining (conj shared item))

               :else
               (recur rest remaining shared))))))
  ([coll1 coll2 & colls]
   (let [smallest-coll-first (sort-by count (conj colls coll1 coll2))]
      (reduce seq-intersection
              (first smallest-coll-first)
              (rest smallest-coll-first)))))

(comment
  (sort-by count [[2 3] [4 5 6] [1]])
  ([1] [2 3] [4 5 6])
  ([1] [2 3] [4 5 6])
  (seq-intersection [1 2 3 3 4] [2 3 4 5 6])
  [2 3 4]

  (seq-intersection [1 2 nil true false] [4 6 nil true false])
  [nil true false]
  []
  (seq-intersection [1 2 true false] [4 6 nil true false])
  [true false]

  (seq-intersection [1 2 ] [3 4 5] [100 nil 3])
  (seq-intersection "abc" "abc")

  )

(defn seq-union
  "Returns a seq that is the union of the input seqs."
  ([s1 s2]
   ;; check if item is in s1
   ;; if it isn't, add it to result
   ;; if it is, remove it from s1
   (loop [[item & rest] s2
          s1 s1
          result s1]
     (cond (and (nil? item)
                (not (seq rest)))
           result

           (some (partial = item) s1)
           (recur rest (seq-disj s1 item) result)

           :else
           (recur rest s1 (conj result item)))))
  ([s1 s2 & seqs]
   (reduce seq-union s1 (conj seqs s2))))

(comment
  (seq-union [1 2 3] [4 5 6])
  [1 2 3 4 5 6]
  (seq-union [1 2 3] [1 2 3])
  [1 2 3]
  (seq-union [1 2 3] [1 2 3 3 3])
  [1 2 3 3 3]
  (seq-union [nil false true] [nil nil false false true true])
  [nil false true nil false true]

  (seq-union [1 2 3] [2 3 4] [3 4 5])
  [1 2 3 4 5]

  )

(defn seq-difference
  "Return a sequence that is the first seq without elements of the remaining seqs."
  ([s1 s2]
   (reduce seq-disj s1 s2))
  ([s1 s2 & seqs]
   (reduce seq-difference  s1 (conj seqs s2))))

(comment
  (reduce seq-disj [1 2 3 4 5] [1 2])
  (seq-difference [1 2 3 4 5] [1 2])
  '(3 4 5)
  (seq-difference [1 2] [1 2 3 4 5])
  '()
  (seq-difference [1 2 3 4 5] [1 2] [5])
  '(3 4)

  )

#_(s/def ::characteristic-relations (s/keys :req [::mem ::ord]))

(defn build-mem
  [coll]
  coll)

(defn build-ob
  "Build the observed-before (ob) relation relating every pair of elements x, y in coll such
  that x occurs before y."
  [coll]
  (loop [[head & rest] coll
         ob []]
    (if head
      (recur rest (into ob (for [x rest] [head x])))
      ob)))

(defn seq->relations
  [s]
  {:mem (build-mem s)
   :ord (build-ob s)})

#_(defn occurs-before?
  "Takes an observed-before tuple and a seq and returns the seq with the relation marked
  if the tuple is true."
  [[a b] seq]
  (let [xxx (split-with )]))

(comment
  (let [[front [target & back]] (split-with (partial not= 1) [1 2 3])]
    (concat front [[::found target]] back))
  ([:paragon.components.mrdt.mrdt/found 1] 2 3)

  (let [[front [target & back]] (split-with (partial not= 1) [1 1 1])
        a-result (concat front [[::found target]] back)
        [front [target & back]] (split-with (partial not= 1) a-result)]
    (concat front [[::found target]] back))
  ([:paragon.components.mrdt.mrdt/found 1] [:paragon.components.mrdt.mrdt/found 1] 1)
  ([:paragon.components.mrdt.mrdt/found 1] 1 1)
  ;; this tagging approach won't work


  )

(defn relations->seq
  [{:keys [mem ord]}]
  ;; start with the mem relation
  ;; for every tuple in ord, check that it is true
  ;; if it isn't swap? shift? make it so
  (reduce (fn [g [a b]]
            (update g a (fnil conj []) b ))
          {}
          ord)
  )
;; step 1 - build directed graph
;; step 2 - sort it topologically
;; step 3 - apply arbitration edge for consistent order


;; L ← Empty list that will contain the sorted elements
;;  in my case, that's nodes that are never second
;; S ← Set of all nodes with no incoming edge

;; while S is not empty do
;;     remove a node n from S
;;     add n to tail of L
;;     for each node m with an edge e from n to m do
;;         remove edge e from the graph
;;         if m has no other incoming edges then
;;             insert m into S

;; if graph has edges then
;;     return error   (graph has at least one cycle)
;; else
;;     return L   (a topologically sorted order)

(comment
  (seq->relations [1 2 3])
  {:mem [1 2 3], :ord [[1 2] [1 3] [2 3]]}
  (relations->seq (seq->relations [1 2 3]))
  {1 [2 3], 2 [3]}

  (seq->relations [1 1 1])
  {:mem [1 1 1], :ord [[1 1] [1 1] [1 1]]}
  (relations->seq (seq->relations [1 1 1]))
  {1 [1 1 1]}

  {1 #{3 2}, 2 #{3}}
  {1 3, 2 3}

  (build-ob [1 1 1])
  [[1 1] [1 1] [1 1]]

  (build-ob [1 2 3])
  [[1 2] [1 3] [2 3]]
  )

(defn seq-product
  "Return the cartesian product of two sets.

  {1, 2} × {red, white, green} = {(1, red), (1, white), (1, green), (2, red), (2, white), (2, green)}."
  [s1 s2]
  (for [a s1 b s2]
    [a b]))

(defn merge-replicas
  "Take the characteristic relations of the lowest common ancestor (lca), replica 1 (r1),
  and replica 2 (r2) and merge them."
  [lca r1 r2]
  (let [{lca-mem :mem lca-ord :ord} lca
        {r1-mem :mem r1-ord :ord} r1
        {r2-mem :mem r2-ord :ord} r2
        merged-mem (seq-union (seq-intersection lca-mem r1-mem r2-mem)
                     (seq-difference r1-mem lca-mem)
                     (seq-difference r2-mem lca-mem))]
    {:mem merged-mem
     :ord (seq-intersection (seq-union (seq-intersection lca-ord r1-ord r2-ord)
                                       (seq-difference r1-ord lca-ord)
                                       (seq-difference r2-ord lca-ord))
                            ;; the new order can only contain merged members
                            (seq-product merged-mem merged-mem))}))

(comment
  (for [a [1 2] b [:red :white :green]]
    [a b])
  ([1 :red] [1 :white] [1 :green] [2 :red] [2 :white] [2 :green])

  (merge-replicas (seq->relations [1 1 1])
                  (seq->relations [1 2 3])
                  (seq->relations [4 5 6]))
  {:mem [2 3 4 5 6], :ord [[2 3] [4 5] [4 6] [5 6]]}

  {:mem [2 3 4 5 6], :ord [[1 2] [1 3] [2 3] [4 5] [4 6] [5 6]]}
  )

(comment
  (let [l (seq->relations [1 2])
        r1 (seq->relations [2])
        r2 (seq->relations [2])]
    (merge-replicas l r1 r2)
    (merge-replicas l r2 r1))
  {:mem [2], :ord []}
  {:mem [2], :ord []}

  ;; ---
  (let [l (seq->relations [1])
        r1 (seq->relations [1 2])
        r2 (seq->relations [1 3])]
    (merge-replicas l r1 r2)
    (merge-replicas l r2 r1))
  {:mem [1 3 2], :ord [[1 3] [1 2]]}
  {:mem [1 2 3], :ord [[1 2] [1 3]]}
  ;; expected ord [[1 2] [1 3] [2 3]] -> where could the [2 3] come from?
  ;; the only place I can think of is the product, but we're intersecting that so it has to be common

  ;; --
  (let [l (seq->relations [1])
        r1 (seq->relations [])
        r2 (seq->relations [])
        r2' (seq->relations [2])]
    (merge-replicas l r1 r2)
    (merge-replicas l r1 r2')
    (merge-replicas l r2' r1))
  {:mem [2], :ord []}
  {:mem [2], :ord []}
  {:mem [], :ord []}


  (let [])

  )
