(ns paragon.services.datahike
  (:require [taoensso.timbre :as log]
            [datahike.api :as dh]
            [clojure.spec.alpha :as s]
            [ghostwheel.core :as gw :refer [>defn =>]]))

(s/def ::backend #{:file})
(s/def ::path string?)
(s/def ::db-uri (s/keys :req-un [::backend ::path]))

(>defn initialize-db!
  "Make sure the database exists, create it if it doesn't."
  [db-uri]
  [::db-uri => any?]
  (log/info "Creating db " db-uri)
  (when (not (dh/database-exists? db-uri))
    (dh/create-database db-uri)))

(>defn connect-db!
  "Create a connection to the database."
  [db-uri]
  [::db-uri => any?]
  (log/info "Connecting db" db-uri)
  (dh/connect db-uri))
