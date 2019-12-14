(ns computer_graphics_coursework_backend.render.vertex
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.render.color :as color])
  (:import (computer_graphics_coursework_backend.math.vector Vector4D Vector3D)
           (java.awt Color)))

(defrecord Vertex
  [position normal texture color output])
(defn is-outside [this] (vec/is-outside (:position this)))

(defn interpolate-scalars
  [v1 v2 v3 b]
  (let [n 0.0]
    (-> n
        (+ (* v1 (b 0))
           (* v2 (b 1))
           (* v3 (b 2))))

    (* n (b 3))))

(defn interpolate-colors
  [v1 v2 v3 b]
  (let [n (Color. 0 0 0 0)]
    (reduce color/sum-colors [n (color/scale-color v1 (b 0))
                              (color/scale-color v2 (b 1))
                              (color/scale-color v3 (b 2))])))

(defn interpolate-vecs-3d
  [v1 v2 v3 b]
  (let [n (Vector3D. 0 0 0)]
    (-> n
        (vec/add (vec/scale-3d v1 (b 0))
                 (vec/scale-3d v2 (b 1))
                 (vec/scale-3d v3 (b 2)))
        (vec/scale-3d (b 3)))))

(defn interpolate-vecs-4d
  [v1 v2 v3 b]
  (let [n (Vector4D. 0 0 0 0)]
    (-> n
        (vec/add (vec/scale-4d v1 (b 0))
                 (vec/scale-4d v2 (b 1))
                 (vec/scale-4d v3 (b 2)))
        (vec/scale-4d (b 3)))))

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

(defn interpolate-vertices
  [v1 v2 v3 b]
  (Vertex. (interpolate-vecs-3d (:position v1) (:position v2) (:position v3) b)
           (vec/normalize (interpolate-vecs-3d (:normal v1) (:normal v2) (:normal v3) b))
           (interpolate-vecs-3d (:texture v1) (:texture v2) (:texture v3) b)
           (interpolate-colors (:color v1) (:color v2) (:color v3) b)
           (interpolate-vecs-4d (:output v1) (:output v2) (:output v3) b)))
