(ns paragon.client)

(defn ^:dev/after-load refresh []
  (println "Reloading app"))

(defn init []
  (refresh))
