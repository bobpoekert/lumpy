(ns cheap.hella.lumpy.core-test
  (:use clojure.test
        cheap.hella.lumpy.core))

(defn random-sequence
  [pool-size]
  (repeatedly (partial rand pool-size)))

(defn eval-comp-pred
  [op pred args]
  (eval (apply inner-comp-pred op pred args)))

(deftest max-min-pred
  (is (= (min-pred (identity) 1 2 3 4 5) (reduce min [1 2 3 4 5])))
  (is (= (max-pred (identity) 1 2 3 4 5) (reduce max [1 2 3 4 5]))))
