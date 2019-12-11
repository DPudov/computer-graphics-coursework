(ns computer_graphics_coursework_backend.render.model
  (:require [compute_-graphics_coursework_backend.render.vertex :as vertex])
  (:import (computer_graphics_coursework_backend.render.vertex SimpleVertex)
           (java.awt Color)))

(defrecord Triangle
  [v0 v1 v2])

(defn build-cube
  [triangle]
  (let [triangles []]
    (conj triangles
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex. ; 7 triangle
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 0 0 1 1) [0 0 1]))
          (Triangle. (SimpleVertex.;8 triangle
                       [-10 -10 10] (Color. 1 1 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 1 1 1 1) [0 0 1])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 1 1 1 1) [0 0 1]))
          (Triangle. (SimpleVertex. ; 9 triangle
                       [-10 -10 10] (Color. 0 1 0 1) [0 -1 0])
                     (SimpleVertex.
                       [10 -10 10] (Color. 0 1 0 1) [0 -1 0])
                     (SimpleVertex.
                       [10 -10 -10] (Color. 0 1 0 1) [0 -1 0]))
          (Triangle. (SimpleVertex.;10 triangle
                       [10 -10 10] (Color. 0 1 0 1) [1 0 0])
                     (SimpleVertex.
                       [10 10 -10] (Color. 0 1 0 1) [1 0 0])
                     (SimpleVertex.
                       [10 10 10] (Color. 0 1 0 1) [1 0 0]))
          (Triangle. (SimpleVertex.; 11 triangle
                       [-10 -10 -10] (Color. 1 1 0 1) [-0.577 -0.577 -0.577])
                     (SimpleVertex.
                       [-10 10 -10] (Color. 1 1 0 1) [-0.577 0.577 -0.577])
                     (SimpleVertex.
                       [-10 -10 10] (Color. 1 1 0 1) [-0.577 -0.577 0.577]))
          (Triangle. (SimpleVertex.; 12 triangle
                       [-10 -10 10] (Color. 1 1 0 1) [-0.577 -0.577 0.577])
                     (SimpleVertex.
                       [-10 10 10] (Color. 1 1 0 1) [-0.577 0.577 0.577])
                     (SimpleVertex.
                       [-10 10 -10] (Color. 1 1 0 1) [-0.577 0.577 -0.577])))))