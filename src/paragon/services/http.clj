(ns paragon.services.http
  (:require [ring.adapter.jetty9 :as ring-http]
            [paragon.server.api :as api]
            [taoensso.timbre :as log]))

(defn start-server [port]
  (log/info "Starting Jetty server on port" port)
  (ring-http/run-jetty api/app {:port port :join? false}))

(defn stop-server [server]
  (log/info "Stopping Jetty server")
  (ring-http/stop-server server))
