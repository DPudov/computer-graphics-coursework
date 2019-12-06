(ns computer-graphics-coursework-backend.render.engine
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.math.matrix :as matr]))

(defn render
  [canvas]
  (let [view-matrix ()
        projection-matrix ()
        world-matrix ()
        transform-matrix ()]))



(definterface Drawable
  (draw [camera meshes]))

(deftype Engine
  [graphics width height depth-buffer image-buffer])
