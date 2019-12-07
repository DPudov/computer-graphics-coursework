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
           (java.awt.event ActionListener)
           (computer_graphics_coursework_backend.math.vector Vector3D Vector4D)))

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

(def Cw 1000)
(def Ch 600)
(def Vw 50)
(def Vh 50)
(def d 10)
(def camera-position (Vector4D. 3 10 0 1))
(def camera-orientation (Vector4D. 0.1 0.3 0.2 1))
(def display-surface (Vector4D. 0 0 1 1))

(defn get-d
  "Camera orientation in radians!"
  [camera-orientation camera-position vertex]
  (let [theta-x (camera-orientation 0)
        theta-y (camera-orientation 1)
        theta-z (camera-orientation 2)]
    (matr/transform (matr/mult (matr/rotate-x theta-x)
                               (matr/rotate-y theta-y)
                               (matr/rotate-z theta-z))
                    (vec/sub vertex camera-position))))

(defn get-screen-point
  [display-surface-e camera-distance-d]
  (let [ex (display-surface-e 0)
        ey (display-surface-e 1)
        ez (display-surface-e 2)
        dx (camera-distance-d 0)
        dy (camera-distance-d 1)
        dz (camera-distance-d 2)]
    (vec [(- (+ (/ (* ez dx 1000) dz 50) ex)) (- (+ (/ (* ez dy 600) dz 50) ey))])))
;(defn viewport-to-canvas
;  [x y]
;  [(/ (* x Cw) Vw) (/ (* y Ch) Vh)])
;
;(defn project-vertex
;  [v]
;  (viewport-to-canvas (/ (* (v 0) d) (v 2)) (/ (* (v 1) d) (v 2))))

(def vAf (Vector4D. -1 1 1 1))
(def vBf (Vector4D. 1 1 1 1))
(def vCf (Vector4D. 1 -1 1 1))
(def vDf (Vector4D. -1 -1 1 1))
(def vAb (Vector4D. -1 1 2 1))
(def vBb (Vector4D. 1 1 2 1))
(def vCb (Vector4D. 1 -1 2 1))
(def vDb (Vector4D. -1 -1 2 1))

(def vertices)

(defn draw-cube
  [canvas]
  (let [vafp (get-screen-point display-surface (get-d camera-orientation camera-position vAf))
        vbfp (get-screen-point display-surface (get-d camera-orientation camera-position vBf))
        vcfp (get-screen-point display-surface (get-d camera-orientation camera-position vCf))
        vdfp (get-screen-point display-surface (get-d camera-orientation camera-position vDf))
        vabp (get-screen-point display-surface (get-d camera-orientation camera-position vAb))
        vbbp (get-screen-point display-surface (get-d camera-orientation camera-position vBb))
        vcbp (get-screen-point display-surface (get-d camera-orientation camera-position vCb))
        vdbp (get-screen-point display-surface (get-d camera-orientation camera-position vDb))]
    (println vafp vbfp vcfp vdfp)
    (drawer/draw-line-fast canvas (int (vafp 0)) (int (vafp 1)) (int (vbfp 0)) (int (vbfp 1)) Color/BLUE)
    (drawer/draw-line-fast canvas (vbfp 0) (vbfp 1) (vcfp 0) (vcfp 1) Color/BLUE)
    (drawer/draw-line-fast canvas (vcfp 0) (vcfp 1) (vdfp 0) (vdfp 1) Color/BLUE)
    (drawer/draw-line-fast canvas (vdfp 0) (vdfp 1) (vafp 0) (vafp 1) Color/BLUE)
    (drawer/draw-line-fast canvas (vabp 0) (vabp 1) (vbbp 0) (vbbp 1) Color/RED)
    (drawer/draw-line-fast canvas (vbbp 0) (vbbp 1) (vcbp 0) (vcbp 1) Color/RED)
    (drawer/draw-line-fast canvas (vcbp 0) (vcbp 1) (vdbp 0) (vdbp 1) Color/RED)
    (drawer/draw-line-fast canvas (vdbp 0) (vdbp 1) (vabp 0) (vabp 1) Color/RED)
    (drawer/draw-line-fast canvas (vafp 0) (vafp 1) (vabp 0) (vabp 1) Color/GREEN)
    (drawer/draw-line-fast canvas (vbfp 0) (vbfp 1) (vbbp 0) (vbbp 1) Color/GREEN)
    (drawer/draw-line-fast canvas (vcfp 0) (vcfp 1) (vcbp 0) (vcbp 1) Color/GREEN)
    (drawer/draw-line-fast canvas (vdfp 0) (vdfp 1) (vdbp 0) (vdbp 1) Color/GREEN)))

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
    (draw-cube canvas)                                      ;(terrain/render-to canvas w))
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (let [canvas (select root [:#canvas])]
    (create-timer (time-ms-by-fps desired-fps) canvas)
    (-> canvas
        (config! :paint paint-frame))))
