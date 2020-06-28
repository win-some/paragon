(ns paragon.services.datahike
  (:require [taoensso.timbre :as log]
            [datahike.api :as dh]
            [clojure.spec.alpha :as s]
            [ghostwheel.core :as gw :refer [>defn]]))


(defn initialize-db
  "Make sure the database exists, create it if it deesn't"
  [& args]
  #_[(s/keys :req-un [::backend ::path])]
  (log/info "Initializing db" args)
  #_(let [main-uri {:backend :file :path "resources/db/main"}]
    (log/info "Creating db " db-uri)
    (when (not (dh/database-exists? main-uri))
      (dh/create-database main-uri))))

(defn connect-db [& args]
  (log/info "Connecting db" args)
  #_(dh/connect {:backend :file :path "resources/dbs/idle"}))
