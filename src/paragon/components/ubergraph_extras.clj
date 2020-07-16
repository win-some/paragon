(ns paragon.components.ubergraph-extras
  (:require [clojure.set :as set]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [ubergraph.core :as uber]
            [ubergraph.alg :as ualg]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn dense-integer-node-labels
  "Given an ubergraph g, returns a map.
  Keys in returned map: :node->int :int->node
  :node->int
  The associated value is a map with the nodes of g as keys, each node
  associated with a distinct integer in the range [0, n-1] where n is
  the number of nodes.
  :int->node
  The associated value is a (mutable) array of objects indexed
  from [0, n-1], and is the reverse mapping of the :node->int map.
  Runs in effectively O(n) time, where n is the number of nodes in the
  graph.
  This is a very simple function, but it is sometimes useful as a step
  before running a more complex algorithm on a graph.  Having a
  contiguous range of integers from [0, n-1] for each node is useful
  for creating an array of values associated with each node, and
  storing them in a vector or Java array separate from the graph data
  structure itself.  If these extra values are no longer needed when
  the algorithm completes, it can be faster to do it this way, versus
  using Ubergraph node attributes.
  Example:
  user=> (require '[ubergraph.core :as uber]
                  '[cljol.ubergraph-extras :as ubere])
  nil
  user=> (def g7 (uber/multidigraph
                  ;; nodes
                  [:node-a {:label \"node a\"}]  :node-b  :node-d  :node-c
                  ;; edges
                  [:node-a :node-b]  [:node-a :node-d]
                  [:node-b :node-d]  [:node-b :node-c]))
  #'user/g7
  user=> (uber/nodes g7)
  (:node-a :node-b :node-d :node-c)
  user=> (def x (ubere/dense-integer-node-labels g7))
  #'user/x
  ;; Demonstration of default way Java array object is shown in REPL
  user=> x
  {:node->int {:node-a 0, :node-b 1, :node-d 2, :node-c 3},
   :int->node #object[\"[Ljava.lang.Object;\" 0x6707382a \"[Ljava.lang.Object;@6707382a\"]}
  ;; Converting Java array contents into a Clojure vector is one way
  ;; to see its contents in the REPL
  user=> (vec (:int->node x))
  [:node-a :node-b :node-d :node-c]"
  [g]
  (let [int->node (object-array (uber/count-nodes g))]
    (loop [i 0
           remaining-nodes (uber/nodes g)
           node->int (transient {})]
      (if-let [s (seq remaining-nodes)]
        (let [n (first s)]
          (aset int->node i n)
          (recur (inc i) (rest remaining-nodes)
                 (assoc! node->int n i)))
        ;; else
        {:node->int (persistent! node->int)
         :int->node int->node}))))


;; topsort2 is unabashedly written in an imperative style with mutable
;; arrays and transients.  This is done in an attempt to achieve the
;; highest performance possible, while being written in Clojure.  I
;; have not checked how close the performance would be if this was
;; written using only immutable data.

;; All mutable values are temporary, being live (i.e. not garbage)
;; only during the execution of topsort2.  Only immutable values are
;; returned.

;; Note 1: The conditional check here is intended to skip updates to
;; sorted-in-edges-temp, if the caller indicates they were not
;; interested in it.  The goal is to maximize performance for the case
;; where the caller does not want this value.

(defn topsort2
  "Given a graph, return a topological ordering of the vertices if
  there are no cycles in the graph, or return a map with value true
  for the key :has-cycle? if there is a cycle.
  https://en.wikipedia.org/wiki/Topological_sorting
  The return value is a map, where the keys depend upon the options
  given, and whether the graph contains a cycle.
  :has-cycle? - boolean
  This key is always present in the returned map.  It is true if a
  cycle was found in the graph, otherwise false.  The cycle found is
  not returned, only the fact that one exists.
  :topological-order - vector of nodes of `graph`
  If no cycle was found, this key is always present, and its value is
  a vector whose elements are the nodes of `graph`, in a topologically
  sorted order, each node appearing exactly once.
  :sorted-in-edges - vector of edge lists
  This key is only present in the returned map if the `opts` map is
  provided with key :sorted-in-edges with a logical true value, and no
  cycle is found in the graph.  When it is present, its associated
  value is a vector.  Let T be the vector that is the value associated
  with the :topological-order key.  Element i of the vector is a list
  of all edges into node (nth T i), where the edges are sorted by
  their source nodes, and those nodes are in reverse topological
  order.
  topsort2 implements Kahn's algorithm.  Pseudocode for Kahn's
  algorithm is given on the Wikipedia page.
  Runs in O((n+m)*C) time, where n is the number of nodes, m is the
  number of edges, and C is the time to do a single lookup or addition
  to a Clojure map, set, or vector of size max(m,n).  C is often
  described as 'effectively constant'.  TBD: Link to details on C.
  Graphs with multiple parallel edges from one node u to another node
  v are supported.
  If a graph has 'self loops', i.e. an edge from a node to itself,
  that is considered a cycle.  Undirected graphs with an edge between
  node u and node v are treated as having directed edges from u to v
  and from v to u, and thus have a cycle."
  ([graph]
   (topsort2 graph {}))
  ([graph opts]
   (let [n (count (uber/nodes graph))
         {^objects i2n :int->node, ^ints n2i :node->int} (dense-integer-node-labels graph)
         sorted-in-edges-temp (when (get opts :sorted-in-edges)
                                (object-array n))
         [^ints in-degree candidates]
         (let [^ints tmp (int-array n)
               n (long n)]
           (loop [i 0
                  candidates ()]
             (if (< i n)
               (let [d (int (uber/in-degree graph (aget i2n i)))]
                 (aset tmp i d)
                 (recur (inc i) (if (zero? d)
                                  (cons i candidates)
                                  candidates)))
               [tmp candidates])))
         t2i (int-array n)

         [final-t T]
         (loop [t 0
                T (transient [])
                candidates candidates]
           (if-let [candidates (seq candidates)]
             (let [i (int (first candidates))
                   u (aget i2n i)]
               (aset t2i t i)
               (let [next-candidates
                     (loop [out-edges (uber/out-edges graph u)
                            cand (next candidates)]
                       (if-let [out-edges (seq out-edges)]
                         (let [e (first out-edges)
                               v (uber/dest e)
                               j (int (n2i v))
                               new-d (dec (aget in-degree j))]
                           (when sorted-in-edges-temp  ;; Note 1
                             (aset sorted-in-edges-temp
                                   j (cons e (aget sorted-in-edges-temp j))))
                           (aset in-degree j new-d)
                           (recur (next out-edges) (if (zero? new-d)
                                                     (cons j cand)
                                                     cand)))
                         ;; else return updated list of candidates
                         cand))]
                 (recur (inc t) (conj! T u) next-candidates)))
             ;; else
             [t (persistent! T)]))]

     (if (< (int final-t) n)
       ;; there is a cycle in the graph, so no topological ordering
       ;; exists.
       {:has-cycle? true}
       (let [sorted-in-edges
             (when sorted-in-edges-temp
               (loop [t 0
                      sorted-in-edges (transient [])]
                 (if (< t n)
                   (let [i (aget t2i t)]
                     (recur (inc t) (conj! sorted-in-edges
                                           (aget sorted-in-edges-temp i))))
                   (persistent! sorted-in-edges))))
             ret {:has-cycle? false, :topological-order T}]
         (if sorted-in-edges
           (assoc ret :sorted-in-edges sorted-in-edges)
           ret))))))


#_(defn remove-loops-and-parallel-edges
  "Given an Ubergraph, return one that has the same nodes and edges,
  except all self loop edges will be removed, and for all pairs of
  nodes that have multiple parallel edges between them, all but one of
  them will be removed.  The one that remains will be chosen
  arbitrarily."
  [graph]
  (let [g (reduce (fn remove-self-loop [g node]
                    (apply uber/remove-edges g (uber/find-edges g node node)))
                  graph (uber/nodes graph))]
    (reduce (fn trim-from-node [g src]
              (reduce (fn trim-from-src-to-dest [g dest]
                        (apply uber/remove-edges
                               g (rest (uber/find-edges g src dest))))
                      g (uber/successors g src)))
            g (uber/nodes g))))


;; Note that dag-transitive-reduction-slow does not actually check
;; whether the input graph is a DAG, but it has been written so that
;; it will give the correct result if the input graph is given a DAG.
;; If not, it might return a graph that is not a transitive reduction
;; of the input graph.

#_(defn dag-transitive-reduction-slow
  [graph]
  (let [g (remove-loops-and-parallel-edges graph)]
    (reduce (fn remove-edge-if-redundant [g edge]
              (let [src (uber/src edge)
                    dest (uber/dest edge)
                    dest? #(= % dest)
                    ;; Pass pre-traverse* a successors function that
                    ;; allows any edge of g to be traversed in the
                    ;; search for a path from src to dest, _except_
                    ;; the one edge directly from src to dest.
                    succ (fn successors-except-by-edge [node]
                           (if (= node src)
                             (remove dest? (uber/successors g node))
                             (uber/successors g node)))
                    reachable-from-src (pre-traverse* succ src)]
                (if (some dest? reachable-from-src)
                  ;; We found a longer way to reach dest from src, so
                  ;; remove the direct edge.
                  (uber/remove-edges g edge)
                  g)))
            g (uber/edges g))))


;; Steps to find transitive reduction of an arbitrary graph:

;; There are multiple choices here, so let us just work out one way to
;; do it that seems reasonably efficient.

;; Start with finding a condensation C of the input graph G.  If G
;; contains cycles, C will have fewer nodes and edges than G, which
;; should make later steps take less time.

;; Find the weakly connected components of C.

;; For each weakly connected component, run Algorithm E from my notes
;; on it, starting with finding a topological ordering of the nodes,
;; and sorting all edges into each node in reverse topological order
;; by source node.  TBD: Can this be done _while_ finding the
;; condensation C?  The topological ordering definitely can, but can
;; the sorting of edges also be done at the same time?

;; In case that sorting of edges is not easy to merge into the
;; condensation code, is it straightforward to do the edge sorting
;; starting with the topological sorting of nodes in C?

#_(defn dag-transitive-reduction
  [graph]
  (let [{:keys [has-cycle? topological-order sorted-in-edges]}
        (topsort2 graph {:sorted-in-edges true})
        T topological-order
        _ (assert (not has-cycle?))
        ;; tbd: create node->int map vertex2t, inverse mapping of T
        ]


    ))

#_(defn check-same-reachability-slow
  "Given two graphs g1 and g2 that have nodes node-set in common with
  each other, check whether among all ordered pairs of nodes (n1, n2)
  in node-set that there is a path from n1 to n2 in g1 if, and only
  if, there is a path from n1 to n2 in g2.
  If you do not specify a node-set, it is assumed that both graphs
  have the same set of nodes, and reachability will be compared for
  all pairs of nodes.
  Note: This function assumes the two graphs both have all nodes in
  node-set in common with each other.  It might behave in an
  undocumented fashion (e.g. return wrong answers, throw an exception)
  if they do not.
  '-slow' is in the name because by design, this function performs its
  task in as simple a way as possible, taking O(n^3 + m*n^2) time,
  where n is the number of nodes in node-set, and m is the maximum
  number of edges between the two graphs.  It is designed not to be
  fast, but to be 'obviously correct', as an aid in checking the
  results of other faster algorithms, such as those that compute a
  minimum equivalent graph, transitive reduction, or transitive
  closure.
  Return a map that always contains the key :pass with an associated
  boolean value.
  If they have the same reachability, the value associated with the
  key :pass is true, otherwise false.
  If they have different reachability, also include the following keys
  in the map returned, describing one difference in reachability found
  between g1 and g2.
  :from-node - value is one node in node-set
  :to-node - value is one node in node-set
  :g1-reaches - boolean value that is true if a path was found
  from (:from-node ret) to (:to-node ret) in graph g1.
  :g2-reaches - boolean value that is true if a path was found
  from (:from-node ret) to (:to-node ret) in graph g2.
  The boolean values associated with keys :g1-reaches and :g2-reaches
  will be different."
  ([g1 g2]
   (check-same-reachability-slow g1 g2 (uber/nodes g1)))
  ([g1 g2 node-set]
   (let [ret (first
              (for [n1 node-set
                    n2 node-set
                    :when (not= n1 n2)
                    :let [n2? #(= % n2)
                          g-reaches? (fn [g]
                                       (some n2?
                                             (pre-traverse*
                                              #(uber/successors g %) n1)))
                          g1-reaches? (g-reaches? g1)
                          g2-reaches? (g-reaches? g2)]
                    :when (not= g1-reaches? g2-reaches?)]
                {:pass false
                 :from-node n1
                 :to-node n2
                 :g1-reaches g1-reaches?
                 :g2-reaches g2-reaches?}))]
     (or ret {:pass true}))))
