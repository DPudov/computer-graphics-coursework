(ns computer_graphics_coursework_backend.render.triangle
  (:require [computer_graphics_coursework_backend.math.vector :as vec]))


(defprotocol TriangleMath
  (normal [this])
  (area [this])
  (fix-normals [this])
  (bounding-box [this])
  (transform [this transform-matrix]))

(defrecord Triangle
  [v1 v2 v3])

(defn is-degenerate [triangle]
  (let [p1 (:position (:v1 triangle))
        p2 (:position (:v2 triangle))
        p3 (:position (:v3 triangle))]
    (or (= p1 p2) (= p1 p3) (= p2 p3))))
;(normal
;  [this]
;  (vec/sub)))

