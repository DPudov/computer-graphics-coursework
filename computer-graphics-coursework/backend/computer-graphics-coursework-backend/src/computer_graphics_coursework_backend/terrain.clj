(ns computer-graphics-coursework-backend.terrain
  (:require [computer-graphics-coursework-backend.matrix :as m]))

(def brightness 150)

(defn saturation
  [saturation-noise grid-size x y z]
  (+ 50 (* 100 noise)))

(defn draw-terrain
  [grid-size terrain-matrix]
  (dotimes [i (- grid-size 1)]
    (dotimes [j (- grid-size 1)]
      (dotimes [k ((terrain-matrix i) j)]
        ()))))
