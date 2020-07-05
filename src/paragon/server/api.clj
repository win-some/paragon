(ns paragon.server.api
  (:require [reitit.ring :as ring]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]
            [ring.middleware.resource :as rmr]
            [reitit.ring.coercion :as coercion]
            [reitit.ring.middleware.muuntaja :as muuntaja]
            [reitit.ring.middleware.parameters :as parameters]
            [muuntaja.core :as m]
            [paragon.server.middleware :as middleware]
            #_[paragon.server.handlers :as handlers]))

(def router
  (ring/router
   [["/api"
     #_["/command" {:post {:handler handlers/command-handler}}]
     #_["/query" {:post {:handler handlers/query-handler}}]]]
   {:data {:muuntaja m/instance
           :middleware [
                        ;; query-params & form-params
                        parameters/parameters-middleware
                        ;; content-negotiation
                        muuntaja/format-negotiate-middleware
                        ;; encoding response body
                        muuntaja/format-response-middleware
                        ;; decoding request body
                        muuntaja/format-request-middleware
                        ;; coercing response bodys
                        coercion/coerce-response-middleware
                        ;; coercing request parameters
                        coercion/coerce-request-middleware
                        ;; log request and response
                        [middleware/wrap-logging :logging]

                        ] }}))

(def app
  (-> (ring/ring-handler
       router
       (fn [_] {:status 200 :body (slurp "resources/public/index.html")}))
      (rmr/wrap-resource "public")))

#_(defn app [request]
    (resp/ok "Hello Paragon!"))
