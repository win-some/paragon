(ns paragon.multiset
  (:refer-clojure :exclude [disj conj]))

(defn multiset
  "Create a multiset from a sequence"
  ([]
   {})
  ([s]
   (frequencies s)))


(defn mset
  "Create a multiset."
  ([]
   {})
  ([& xs]
    (multiset xs)))

(defn multiset-reader
  [x]
  (multiset x))

(defn conj
  "con[join]. Returns a multiset with the key incremented."
  [ms k]
  (update ms k (fnil inc 0)))

(defn disj
  "dis[join]. Returns a multiset with the key removed. If there are multiple instances of
  the key, only one instance is removed."
  [ms k]
  (let [v (get ms k )]
    (if (= 1 v)
      (dissoc ms k)
      (update ms k dec))))

(defn union
  "Returns a multiset that is the result of unioning the multisets together."
  ([ms1 ms2]
   (loop [[entry & rest] ms2
          result ms1]
     (if (seq entry)
       (let [[k v] entry]
         (recur rest (update result k (fnil (partial + v) 0))))
       result)))
  ([ms1 ms2 & multisets]
   (reduce union ms1 (clojure.core/conj multisets ms2))))

(defn intersection
  "Returns only the common keys of the multisets."
  ([ms1 ms2]
   (if (< (count ms2) (count ms1))
     (recur ms2 ms1)
     (loop [[entry & rest] ms2
            result ms1]
       (if (seq entry)
         (let [[k v2] entry
               v1 (get ms1 k 0)
               new-v (min v1 v2)]
           (recur rest (if (zero? new-v)
                         (dissoc result k)
                         (assoc result k new-v ))))
         result))))
  ([ms1 ms2 & multisets]
   (let [smallest-first (sort-by count (clojure.core/conj multisets ms1 ms2))]
     (reduce intersection (first smallest-first) (rest smallest-first)))))

(defn difference
  "Returns the keys in ms1 that are not in the following multisets."
  ([ms1 ms2]
   (reduce disj ms1 (keys ms2)))
  ([ms1 ms2 & multisets]
   (reduce difference ms1 (conj multisets ms2))))

(defn product
  "Returns the cartesian product of two sets."
  [ms1 ms2]
  (multiset (for [a (seq ms1) b (seq ms2)]
              [a b])))

(comment
  (multiset [1 2 3 1 2 3 nil true false ])
  {1 2, 2 2, 3 2, nil 1, true 1, false 1}

  {1 2} {1 2}
  (for [a [1 2 3] b [4 5 6]]
    [a b])
  (product (multiset [1 1 1]) (multiset [4 5 6]))
  ([[1 3] [4 1]] [[1 3] [5 1]] [[1 3] [6 1]])
  (reduce
   (fn [res [[1 1] [4 1]]])
   {}
   '([[1 1] [4 1]] [[1 1] [5 1]] [[1 1] [6 1]] [[2 1] [4 1]] [[2 1] [5 1]] [[2 1] [6 1]] [[3 1] [4 1]] [[3 1] [5 1]] [[3 1] [6 1]]))

  ([1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6])

  (difference (multiset [1 1 2]) (multiset))
  {1 1}

  (intersection (multiset [1 1 1 2 3]) (multiset [1 2 3]))
  {1 1, 2 1, 3 1}
  (intersection (multiset [1 1 1 2 3]) (multiset [1 2 3])
                (multiset [1 2 3]))
  {1 1, 2 1, 3 1}
  (intersection (multiset [1 1 1 2 3]) (multiset [1 2 3])
                (multiset))
  (intersection (multiset [1 1 1 2 3]) (multiset))
  {1 3, 2 1, 3 1}
  {1 1, 2 1, 3 1}

  (disj (conj (multiset) 1) 1)
  ()

  (union (multiset [1 2 3]) (multiset [4 5 6]))
  (union (multiset [1 2 3]) (multiset [1 4 5])
         (multiset [1 6 7]))



  {1 1, 2 1, 3 1, 4 1, 5 1, 6 1}
  (concat
   {1 5, 3 1}
   {1 3, 3 1})
  ([1 5] [3 1] [1 3] [3 1])


  )
