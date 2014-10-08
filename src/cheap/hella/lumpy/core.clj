(ns cheap.hella.lumpy.core
  (require [clojure.core.matrix :as m]))

(defn median
  "Takes the median row of a core.matrix matrix"
  ([sorter v]
    (let [s (m/slices v)]
      (nth (sorter s) (Math/floor (/ (count s) 2)))))
  ([v]
    (median sort v)))

(defn inner-comp-pred
  ([op expr-map k x] [expr-map x])
  ([op expr-map k x y]
    (let [pexpr (concat k (list x))
          nom (get expr-map pexpr (gensym))]
      [(assoc expr-map pexpr nom)
       `(if (~op ~nom ~(concat k (list y))) ~x ~y)]))
  ([op expr-map k x y & more]
    (reduce
      (fn [[expr-map res] arg]
        (let [[vmap res2] (inner-comp-pred op k res arg)]
          [(merge vmap expr-map) res2]))
      (inner-comp-pred op expr-map k x y)
      more)))

(defn comp-pred
  [op k args]
  (let [[expr-map res] (apply inner-comp-pred (concat (list op {} k) args))]
    `(let ~(into [] (interleave (vals expr-map) (keys expr-map)))
      ~res)))

(defmacro max-pred
  "Inlined max-key.
   Takes an expr instead of a fn, where the thing to be compared gets appended to the end."
  [k & args]
  (comp-pred '< k args))

(defmacro min-pred
  "Inlined min-key.
   Takes an expr instead of a fn, where the thing to be compared gets appended to the end."
  [k & args]
  (comp-pred '> k args))

(comp-pred '< '(identity) [1 2 3 4])
