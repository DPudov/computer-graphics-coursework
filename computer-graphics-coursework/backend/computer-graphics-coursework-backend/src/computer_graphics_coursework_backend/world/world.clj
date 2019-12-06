(ns computer_graphics_coursework_backend.world.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.math.matrix :as matr]
            [computer-graphics-coursework-backend.render.engine :as engine]
            [computer_graphics_coursework_backend.render.drawer :as drawer]
            [computer-graphics-coursework-backend.world.terrain :as terrain]
            [computer-graphics-coursework-backend.world.perlin :as perlin])
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
  (get-current-simulation-time [this] "return aggregated simulation")
  (inc-simulation-time [this amount])
  (update-water [this]))

(defn hsb-to-rgb
  [hue saturation brightness]
  (Color/HSBtoRGB hue saturation brightness))

(def matrix-stack
  '())

(def matrix-context)

(defn push-matrix
  (conj matrix-stack matrix-context))

(defn pop-matrix
  (var-set matrix-context (peek matrix-stack))
  (var-set matrix-stack (pop matrix-stack)))

;(push-matrix)
;(fill stroke-color)
;(translate (+ (- w2) (* i voxel-width)))
;(pop-matrix)))))))


;
;(deftype Scene
;  [terrain water energy simulation-time]
;  SceneAPI
;  (get-terrain [this]
;    (:terrain this))
;
;  (get-water [this]
;    (:water this))
;
;  (get-energy [this]
;    (:energy this))
;
;  (get-current-simulation-time [this]
;    (:simulation-time this))
;
;  (inc-simulation-time [this amount]
;    (set! simulation-time (+ simulation-time amount)))
;
;  (update-water [this]))

(defn create-timer
  [tick-time canvas]
  (Timer. tick-time (reify ActionListener
                      (actionPerformed [this e]
                        (.repaint canvas)))))

(defn paint-frame [c g]
  (let [w (.getWidth c) w2 (/ w 2)
        h (.getHeight c) h2 (/ h 2)
        canvas (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (terrain/render-to canvas w)
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (let [canvas (select root [:#canvas])]
    (create-timer (time-ms-by-fps desired-fps) canvas)
    (-> canvas
        (config! :paint paint-frame))))
