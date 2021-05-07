(ns paragon.components.mrdt.graph)

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

(defn topo-sort
  "Topographically sort a tree, using the ordering function to provide a consistent total
  order in cases where a node has multiple outgoing edges. Not stack safe. Not type safe."
  ([g]
   ;; this will blow up on non-number collections
   (topo-sort compare g))
  ([ord g]
   (topo-sort ord (prune g) (->> (vals g)
                                 ;; there should only be one "root" node that doesn't have
                                 ;; any incoming edges
                                 (filter (complement :in))
                                 (first)
                                 (:val))))
  ([ord g start]
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


  ({:val 1, :out #{4 3 2}})


  )
