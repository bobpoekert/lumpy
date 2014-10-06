(ns cheap.hella.clumpy.indexing)

(defprotocol spatial-index
  (nearest-neighbors [this k target])
  (ball-search [this radius target]))
