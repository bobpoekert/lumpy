(ns cheap.hella.lumpy.indexing
  (require [clojure.core.matrix :as m]
           [cheap.hella.lumpy.core :as c]
           [cheap.hella.lumpy.indexing :as ix]))

(set! *warn-on-reflection* true)

(defrecord KDTreeNode [location left-child right-child])

(defn kd-tree
  ([points] (kd-tree 0 (m/dimensionality points) (m/slices points)))
  ([depth k slices]
    (let [axis (mod depth k)
          len (count slices)
          sorted (sort-by (partial m/mget axis) slices)
          idx (quot len 2)]
      (case len
        0 nil
        1 (->KDTreeNode (first slices) nil nil)
        (->KDTreeNode
          (nth sorted idx)
          (kd-tree (inc depth) k (take idx sorted))
          (kd-tree (inc depth) k (drop (inc idx) sorted)))))))

(defn tree-distance-fn
  [distance-fn]
  (fn [^KDTreeNode left ^KDTreeNode right]
    (distance-fn (.location left) (.location right))))

(def euclidean-distance
  (tree-distance-fn m/distance))

(defn get-zip
  [^KDTreeNode tree distance-fn target]
  (if (and (nil? (.left-child tree)) (nil? (.right-child tree)))
    (list tree)
    (let [subtree (c/min-pred (distance-fn target) tree (.left-child tree) (.right-child tree))]
      (if (= subtree tree)
        (list (.location tree))
        (cons (.location tree) (get-zip subtree distance-fn target))))))
