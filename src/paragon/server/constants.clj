(ns paragon.server.constants)

(def redacted-keys
  "Request keys whose values we don't want to log."
  #{:password})
