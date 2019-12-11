(ns computer_graphics_coursework_backend.render.engine
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.math.matrix :as matr]
            [computer_graphics_coursework_backend.render.depthbuffer :as depth])
  (:import (java.awt.image BufferedImage)
           (java.awt Color)))

(defrecord State
  [^BufferedImage canvas
   depth-buffer
   model
   camera
   light])

(def background-color (atom Color/WHITE))

(defn clear-render-buffer
  [^BufferedImage canvas]
  (let [g (.getGraphics canvas)
        w (.getWidth canvas)
        h (.getHeight canvas)]
    (.setBackground g @background-color)
    (.clearRect g 0 0 w h)))


(defn render
  [state]
  (let [render-buffer (clear-render-buffer (:canvas state))
        depth-buffer (depth/clear-depth-buffer (:depth-buffer state))
        voxels (:model state)
        camera (:camera state)
        light (:light state)]))




(definterface Drawable
  (draw [camera meshes]))

(deftype Engine
  [graphics width height depth-buffer image-buffer])
