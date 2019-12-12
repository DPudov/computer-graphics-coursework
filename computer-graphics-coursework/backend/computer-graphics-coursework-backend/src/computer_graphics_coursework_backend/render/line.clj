(ns computer-graphics-coursework-backend.render.line
  (:require [computer_graphics_coursework_backend.render.vertex :as vertex])
  (:import (computer_graphics_coursework_backend.render.vertex Vertex)))

(defrecord Line
  [v1 v2])

(defn create-line
  [v1 v2]
  (Line. v1 v2))

(defn create-from-points
  [p1 p2]
  (let [v1 (Vertex. p1 nil nil nil nil)
        v2 (Vertex. p2 nil nil nil nil)]
    (create-line v1 v2)))

