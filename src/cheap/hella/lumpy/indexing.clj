(ns cheap.hella.lumpy.indexing
  (require [clojure.core.matrix :as m]
           [cheap.hella.lumpy.core :as c]))

(set! *warn-on-reflection* true)

(defprotocol SpatialIndex
  (nearest-neighbors [this k target])
  (ball-search [this radius target]))

(defrecord KDTreeNode [location left-child right-child])

(defn kd-tree
  ([points] (kd-tree 0 (m/dimensionality points) points))
  ([depth k points]
    (let [axis (mod depth k)
          slices (m/slices points)
          sorted (sort-by (partial m/mget axis) slices)
          idx (quot (count slices) 2)]
      (when-not (empty? slices)
        (->KDTreeNode
          (nth sorted idx)
          (kd-tree (inc depth) (take idx sorted))
          (kd-tree (inc depth) (drop idx sorted)))))))
