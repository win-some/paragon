(ns paragon.multiset
  (:require [clojure.pprint :as pprint])
  (:import (java.util Collection)
           (clojure.lang IPersistentMap)))

(defprotocol Multiplicities
  (multiplicities [this]))

(deftype Multiset [^IPersistentMap meta
                   ^IPersistentMap multiplicities
                   ^int size]
  clojure.lang.IPersistentSet
  (get [this k]
    (if-let [e (find multiplicities k)]
      (key e)))
  (contains [this k]
    (boolean (find multiplicities k)))
  (disjoin [this k]
    (if-let [v (get multiplicities k)]
      (->Multiset
       meta
       (if (= 1 v)
         (dissoc multiplicities k)
         (update multiplicities k dec))
       (dec size))
      this)
    )

  clojure.lang.IPersistentCollection
  (cons [this k]
    (->Multiset
     meta
     (update multiplicities k (fnil inc 0))
     (inc size)))
  (empty [this] (with-meta (->Multiset nil {} 0) meta))
  (equiv [this x] (.equals this x))

  clojure.lang.Seqable
  (seq [this]
    (if-let [entry (first (seq multiplicities))]
      (let [k (key entry)]
        (lazy-seq (cons k (.seq (.disjoin this k)))))))

  clojure.lang.Counted
  (count [this] size)

  clojure.lang.IMeta
  (meta [this] meta)

  clojure.lang.IObj
  (withMeta [this m]
    (->Multiset m multiplicities size))

  Object
  (equals [this x]
    (if (instance? Multiset x)
      (.equals multiplicities (.multiplicities ^Multiset x))
      false))
  (hashCode [this]
    (hash-combine (hash multiplicities) Multiset))
  (toString [this] (str "#mset " `[~@(seq this)]))

  clojure.lang.IFn
  (invoke [this k]
    (.get this k))
  (invoke [this k default]
    (if-let [r (.get this k)]
      r
      default))

  Collection
  (isEmpty [this]
    (zero? size))
  (size [this] size)
  (^objects toArray [this ^objects a]
   (.toArray ^Collection (or (seq this) ()) a))
  (toArray [this]
    (.toArray ^Collection (or (seq this) ())))
  (iterator [this]
    (.iterator ^Collection (or (seq this) ())))
  (containsAll [this coll]
    (.containsAll ^Collection (into #{} this) coll))

  Multiplicities
  (multiplicities [this] multiplicities)
  )

(defmethod print-method Multiset
  [^Multiset mset ^java.io.Writer w]
  (.write w (.toString mset)))

(defmethod pprint/simple-dispatch Multiset
  [^Multiset mset]
  (pr mset))

(comment
  (= (Multiset. nil {} 0)
     (Multiset. nil {} 0))
  (empty #mset [1 1 2])
  (conj #mset [] 1)
  (conj #mset [1] 1)
  (conj #mset [1 1] 3)
  (disj #mset [1 1 3] 3)

  (count #mset [1  3])





  )

(defn multiset
  "Create a multiset from a sequence"
  ([]
   {})
  ([s]
   (->Multiset nil (frequencies s) (count s))))

(defn mset
  "Create a multiset."
  ([]
   {})
  ([& xs]
   (multiset xs)))

(defn multiset-reader
  [x]
  (multiset x))

(defn multiset?
  [x]
  (instance? Multiset x))

(defn union
  "Returns a multiset that is the result of unioning the multisets together."
  ([ms1 ms2]
   (loop [[entry & rest] (multiplicities ms2)
          result ms1]
     (if (seq entry)
       (let [[k v] entry]
         (recur rest (->Multiset nil
                                 (update (multiplicities result) k (fnil (partial + v) 0))
                                 (+ (count result) v))))
       result)))
  ([ms1 ms2 & multisets]
   (reduce union ms1 (clojure.core/conj multisets ms2))))

(comment
  (union #mset [1 1 2 3] #mset [1 6 7])
  #mset [1 1 1 2 3 6 7]

  )

(defn intersection
  "Returns only the common keys of the multisets."
  ([ms1 ms2]
   (if (< (count ms2) (count ms1))
     (recur ms2 ms1)
     (loop [[entry & rest] (multiplicities ms2)
            result ms1]
       (if (seq entry)
         (let [[k v2] entry
               v1 (get ms1 k 0)
               new-v (min v1 v2)]
           (recur rest (->Multiset nil (if (zero? new-v)
                                         (dissoc (multiplicities result) k)
                                         (assoc (multiplicities result) k new-v))
                                   (- (count result) (- v1 new-v)))))
         result))))
  ([ms1 ms2 & multisets]
   (let [smallest-first (sort-by count (conj multisets ms1 ms2))]
     (reduce intersection (first smallest-first) (rest smallest-first)))))

(comment
  (count (intersection #mset [1 1 2 3] #mset [1 4]))
  )

(defn difference
  "Returns the keys in ms1 that are not in the following multisets."
  ([ms1 ms2]
   (reduce disj ms1 (keys (multiplicities ms2))))
  ([ms1 ms2 & multisets]
   (reduce difference ms1 (conj multisets ms2))))

(comment
  (difference #mset [] #mset [1 1 3])

  )

(defn product
  "Returns the cartesian product of two sets."
  [ms1 ms2]
  (for [a (seq ms1) b (seq ms2)]
    [a b]))

(comment
  (multiset [1 2 3 1 2 3 nil true false])

  (for [a [1 2 3] b [4 5 6]]
    [a b])
  (product (multiset [1 1 1]) (multiset [4 5 6]))
  ([1 4] [1 5] [1 6] [1 4] [1 5] [1 6] [1 4] [1 5] [1 6])
  ([[1 3] [4 1]] [[1 3] [5 1]] [[1 3] [6 1]])
  (reduce
   (fn [res [[1 1] [4 1]]])
   {}
   '([[1 1] [4 1]] [[1 1] [5 1]] [[1 1] [6 1]] [[2 1] [4 1]] [[2 1] [5 1]] [[2 1] [6 1]] [[3 1] [4 1]] [[3 1] [5 1]] [[3 1] [6 1]]))

  ([1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6])

  (difference (multiset [1 1 2]) (multiset))
  {1 1}

  #{1 2 3}
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
