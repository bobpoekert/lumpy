(ns cheap.hella.lumpy.indexing)

(defprotocol SpatialIndex
  (nearest-neighbors [this k target])
  (ball-search [this radius target]))

