(ns paragon.main
  (:require [taoensso.timbre :as log]))

(defn go
  [arg]
  arg)

(defn -main []
  (log/info "all systems nominal")
  (go :ready))
