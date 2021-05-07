(ns paragon.playground
  (:require [manifold.stream :as stream]
            [clojure.spec.alpha :as s]
            [malli.core :as m]))

(m/validate [:and int? [:< 6]] 7)

(def s (stream/stream))

(comment
  @(stream/put! s 1)

  @(stream/take! s 1)
  (stream/close! s)

  (->> [1 2 3]
       (stream/transform (map inc))
       (stream/stream->seq))




  )
