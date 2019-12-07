(ns computer-graphics-coursework-backend.render.triangle
  (:require [computer_graphics_coursework_backend.math.vector :as vec])
  (:import (computer_graphics_coursework_backend.render.triangle TriangleMath)))

(defprotocol CheckDegenerate
  (is-degenerate [this]))

(defprotocol TriangleMath
  (normal [this])
  (area [this])
  (fix-normals [this])
  (bounding-box [this])
  (transform [this transform-matrix]))

(defrecord Triangle
  [v1 v2 v3]
  CheckDegenerate
  (is-degenerate [this]
    (let [p1 (:position v1)
          p2 (:position v2)
          p3 (:position v3)]
      (or (= p1 p2) (= p1 p3) (= p2 p3)))))
  ;(normal
  ;  [this]
  ;  (vec/sub)))

