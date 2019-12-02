(ns computer_graphics_coursework_backend.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [computer_graphics_coursework_backend.vector :as vec]
            [computer_graphics_coursework_backend.matrix :as matr]
            [computer-graphics-coursework-backend.engine :as engine]
            [computer_graphics_coursework_backend.drawer :as drawer])
  (:import (java.awt.image BufferedImage)
           (java.awt Color)
           (javax.swing Timer)
           (java.awt.event ActionListener)))

(def desired-fps 30)

(defn time-ms-by-fps
  [fps]
  (/ 1000 fps))

(defprotocol SceneAPI
  (get-terrain [this] "get terrain as 3d array")
  (get-water [this] "get water as 3d array")
  (get-energy [this] "get water energy")
  (get-current-simulation-time [this] "return aggregated simulation"))

(defrecord Scene
  [terrain water energy]
  SceneAPI
  (get-terrain [this])
  (get-water [this])
  (get-energy [this])
  (get-current-simulation-time [this]))

(defn create-timer
  [tick-time canvas]
  (Timer. tick-time (reify ActionListener
                      (actionPerformed [this e]
                        (.repaint canvas)))))

(defn paint-frame [c g]
  (let [w (.getWidth c) w2 (/ w 2)
        h (.getHeight c) h2 (/ h 2)
        canvas (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    ()
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (let [canvas (select root [:#canvas])]
    (create-timer (time-ms-by-fps desired-fps) canvas)
    (-> canvas
        (config! :paint paint-frame))))
