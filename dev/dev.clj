(ns dev
  (:require [clojure.repl :refer :all]
            [paragon.services.config :as config]
            [juxt.clip.repl :as clipr :refer [start stop reset set-init! system]]))

(println "(start) to start; (reset) to reset; (stop) to stop;")

(set-init! #(:juxt.clip/system (config/read-config :dev)))
