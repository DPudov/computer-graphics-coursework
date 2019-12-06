(ns computer-graphics-coursework-backend.render.vertex
  (:require [computer_graphics_coursework_backend.math.vector :as vec])
  (:import (computer_graphics_coursework_backend.math.vector Vector4D)))

(defprotocol Outsider
  (is-outside []))

(defrecord Vertex
  [position normal texture color output]
  Outsider
  (is-outside [] (vec/is-outside output)))

(defn barycentric
  [p1 p2 p3 p]
  (let [v0 (vec/sub-3d p2 p1)
        v1 (vec/sub-3d p3 p1)
        v2 (vec/sub-3d p p1)
        d00 (vec/dot-3d v0 v0)
        d01 (vec/dot-3d v0 v1)
        d11 (vec/dot-3d v1 v1)
        d20 (vec/dot-3d v2 v0)
        d21 (vec/dot-3d v2 v1)
        d (- (* d00 d11) (* d01 d01))
        v (/ (- (* d11 d20) (* d01 d21)) d)
        w (/ (- (* d00 d21) (* d01 d20)) d)
        u (- 1 v w)]
    (Vector4D. u v w 1)))