(ns paragon.services.config
  (:require [aero.core :as aero]
            [clojure.java.io :as io]
            [ghostwheel.core :as gw :refer [>defn =>]]))


(>defn read-config
  "Read the system config from the classpath, with values for the given profile."
  [profile]
  [keyword? => map?]
  (aero/read-config (io/resource "system-config.edn") {:profile profile}))
