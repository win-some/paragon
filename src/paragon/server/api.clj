(ns paragon.server.api
  (:require [ring.util.http-response :as resp]))

(defn app [request]
  (resp/ok "Hello Paragon!"))
