(ns cheap.hella.lumpy.core
  (require [clojure.core.matrix :as m]))

(defn median
  "Takes the median row of a core.matrix matrix"
  ([sorter v]
    (let [s (m/slices v)]
      (nth (sorter s) (Math/floor (/ (count s) 2)))))
  ([v]
    (median sort v)))
