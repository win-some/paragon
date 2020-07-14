(ns paragon.components.mrdt.mrdt-test
  (:require [clojure.test :refer [deftest testing is]]
   [expectations.clojure.test :as e :refer [defexpect]]
            [paragon.components.mrdt.mrdt :as mrdt]))

(deftest queue-mergin
  ;; From Fig 6. in MRDTs
  (testing "Fig 6a"
    (let [l (mrdt/seq->relations [1 2])
          ;; each replica has `pop`ed the first el
          r1 (mrdt/seq->relations [2])
          r2 (mrdt/seq->relations [2])]
      (e/expect {:mem [2] :ord []}
                (mrdt/merge-replicas l r1 r2))))

  (testing "Fig 6b"
    (let [l (mrdt/seq->relations [1])
          ;; r1 pushes 2
          r1 (mrdt/seq->relations [1 2])
          ;; r2 pushes 3
          r2 (mrdt/seq->relations [1 3])]
      (e/expect {:mem [1] :ord []} l)
      (e/expect {:mem [1 2] :ord [[1 2]]} r1)
      (e/expect {:mem [1 3] :ord [[1 3]]} r2)

      ;; merge r2 into r1
      (e/expect {:mem [1 2 3] :ord [[1 2] [1 3] [2 3]]}
                (mrdt/merge-replicas l r1 r2))
      ;; merge r1 into r2
      (e/expect {:mem [1 2 3] :ord [[1 2] [1 3] [2 3]]}
                (mrdt/merge-replicas l r2 r1))))

  #_(testing "Fig 6c"
    (let [l (mrdt/seq->relations [1])
          ;; r1 pops
          r1 (mrdt/seq->relations [])
          ;; r2 pops
          r2 (mrdt/seq->relations [])]
      (e/expect {:mem [1] :ord []} l)
      (e/expect {:mem [1 2] :ord [[1 2]]} r1)
      (e/expect {:mem [1 3] :ord [[1 3]]} r2)

      ;; merge r2 into r1
      (e/expect {:mem [1 2 3] :ord [[1 2] [1 3] [2 3]]}
                (mrdt/merge-replicas l r1 r2))
      ;; merge r1 into r2
      (e/expect {:mem [1 2 3] :ord [[1 2] [1 3] [2 3]]}
                (mrdt/merge-replicas l r2 r1)))))
