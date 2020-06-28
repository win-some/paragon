(ns dev
  (:require [clojure.repl :refer :all]
            [paragon.services.config :as config]
            [juxt.clip.repl :as clipr :refer [start stop reset set-init! system]]))

(set-init! #(config/read-config :dev))

(config/read-config :dev)
