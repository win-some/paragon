(ns paragon.main-test
  (:require [paragon.main :as main]
            [clojure.test :refer :all]
            [expectations.clojure.test :as e :refer [defexpect]]))

(deftest go
  (e/expect :go (main/go)))
