(ns paragon.server.middleware
  "A namespace for defining api global middleware."
  (:require [taoensso.timbre :as log]
            [paragon.server.constants :as constant]))

(defn wrap-exception
  "Catch any exceptions generated during handling and return a 500 response."
  [handler id]
  (fn [request]
    (try (handler request)
         (catch Exception e
           (log/info e)
           {:status 500 :body (str e)}))))

(defn redact
  "Replace all redacted field values, recursively, with a redacted value."
  [request]
  (reduce (fn [redacted [key val]]
            (cond (constant/redacted-keys key)
                  (assoc redacted key (str "***REDACTED-" (.toUpperCase (name key)) "***"))
                  (map? val)
                  (assoc redacted key (redact val))
                  :else
                  (assoc redacted key val)))
          {}
          request))

(defn wrap-logging
  "Log the redacted request (without the garbage) and the response."
  [handler id]
  (fn [request]
    (let [req-log (dissoc request
                          :reitit.core/match :reitit.core/router :server-exchange
                          :body :muuntaja/request :muuntaja/response)
          response (handler request)]
      (log/info "request" (redact req-log))
      ;; don't redact response in case we have validation errors
      (log/info "response" response)
      response)))
