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

(def terrain-noise 5)

(def hue-noise 3)

(def saturation-noise 3)



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

(defn wall-level
  [dim]
  (* dim 100))

(defn parabolic-height
  [dim i j]
  (- (* 0.4 dim) (* 0.02 (+ (* (- i (/ dim 2)) (- i (/ dim 2))) (* (- j (/ dim 2)) (- j (/ dim 2)))))))

(defn fade
  [t]
  (* t t t (+ (* t (- (* t 6) 15)) 10)))

(defn lerp
  [t a b] (+ a (* t (- b a))))

(defn grad
  [^int hash x y z]
  (let [h (bit-and hash 15)
        u (if (< h 8) x y)
        v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))]
    (+ (if (= (bit-and h 1) 0) x z)
       (if (= (bit-and h 2) 0) v (- v)))))

(defn noise
  [x y z])





(defn noised-height
  [dim i j terrain-noise]
  (* 0.5 dim (+ -0.5 (noise (/ (* terrain-noise i) dim)
                            (/ (* terrain-noise j) dim)
                            100))))

(defn calculate-height
  [parabolic noised]
  (max 0
       (int (+ parabolic noised))))

(deftype Terrain
  [dim level-matrix])
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
    ()
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (let [canvas (select root [:#canvas])]
    (create-timer (time-ms-by-fps desired-fps) canvas)
    (-> canvas
        (config! :paint paint-frame))))
