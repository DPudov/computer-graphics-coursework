(ns computer-graphics-coursework-backend.engine
  (:require [computer_graphics_coursework_backend.vector :as vec]
            [computer_graphics_coursework_backend.matrix :as matr]))

(defn render
  [camera meshes]
  (let [view-matrix ()
        projection-matrix ()
        world-matrix ()
        transform-matrix ()]))



(definterface Drawable
  (draw [camera meshes]))

(deftype Engine
  [graphics width height depth-buffer image-buffer])
