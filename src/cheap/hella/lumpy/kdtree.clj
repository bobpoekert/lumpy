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

(defn get-zip
  [^KDTreeNode tree distance-fn target]
  (cond
    (and (nil? (.left-child tree)) (nil? (.right-child tree))) (list tree)
    (nil? (.left-child tree)) (if (> (distance-fn target (.location tree))
                                     (distance-fn target (.location (.right-child tree))))
                                (cons tree (get-zip (.right-child tree) distance-fn target))
                                (list tree))
    (nil? (.right-child tree)) (if (> (distance-fn target (.location tree))
                                      (distance-fn startet (.location (.left-child tree))))
                                (cons tree (get-zip (.left-child tree) distance-fn target))
                                (list tree))
    :else (let [dl (distance-fn target (.location (.left-child tree)))
                dr (distance-fn target (.location (.right-child tree)))]
            (case (compare dl dr)
              -1 (cons tree (get-zip (.left-child tree) distance-fn target))
              0 (list tree)
              1 (cons tree (get-zip (.right-child tree) distance-fn target))))))
