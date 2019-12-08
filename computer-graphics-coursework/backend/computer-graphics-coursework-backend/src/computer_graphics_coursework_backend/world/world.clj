(ns computer_graphics_coursework_backend.world.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.math.matrix :as matr]
            [computer-graphics-coursework-backend.render.engine :as engine]
            [computer_graphics_coursework_backend.render.camera :as camera]
            [computer_graphics_coursework_backend.render.drawer :as drawer]
            [computer-graphics-coursework-backend.world.terrain :as terrain]
            [computer-graphics-coursework-backend.world.perlin :as perlin])
  (:import (java.awt.image BufferedImage)
           (java.awt Color)
           (javax.swing Timer)
           (java.awt.event ActionListener KeyEvent)
           (computer_graphics_coursework_backend.math.vector Vector3D Vector4D)
           (computer_graphics_coursework_backend.math.matrix Matrix4D)))

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


(def model-matrix
  (Matrix4D. 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))
(def vAf (Vector4D. -1 1 1 1))
(def vBf (Vector4D. 1 1 1 1))
(def vCf (Vector4D. 1 -1 1 1))
(def vDf (Vector4D. -1 -1 1 1))
(def vAb (Vector4D. -1 1 3 1))
(def vBb (Vector4D. 1 1 3 1))
(def vCb (Vector4D. 1 -1 3 1))
(def vDb (Vector4D. -1 -1 3 1))

(defn draw-cube-another
  [canvas camera-position]
  (let [mvp (camera/model-view-projection-matrix camera/perspective
                                                 (camera/view-matrix camera-position @camera/target camera/up)
                                                 model-matrix)
        viewport [(.getWidth canvas) (.getHeight canvas)]
        vafp (camera/project-to-screen vAf mvp viewport)
        vbfp (camera/project-to-screen vBf mvp viewport)
        vcfp (camera/project-to-screen vCf mvp viewport)
        vdfp (camera/project-to-screen vDf mvp viewport)
        vabp (camera/project-to-screen vAb mvp viewport)
        vbbp (camera/project-to-screen vBb mvp viewport)
        vcbp (camera/project-to-screen vCb mvp viewport)
        vdbp (camera/project-to-screen vDb mvp viewport)]
    (drawer/draw-line-fast canvas (vafp 0) (vafp 1) (vbfp 0) (vbfp 1) Color/BLUE)
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

(defn create-timer
  [tick-time canvas]
  (let [delta-angle (Vector3D. 0 0.5 0)]
    (Timer. tick-time (reify ActionListener
                        (actionPerformed [this e]
                          (swap! camera/camera-position camera/rotate-camera delta-angle)
                          (.repaint canvas))))))

(defn clear [canvas]
  (let [g (.getGraphics canvas)
        w (.getWidth canvas)
        h (.getHeight canvas)]
    (.setBackground g Color/WHITE)
    (.clearRect g 0 0 w h)))


(defn paint-frame [c g]
  (let [w (.getWidth c) w2 (/ w 2)
        h (.getHeight c) h2 (/ h 2)
        canvas (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)]
    (clear canvas)
    (draw-cube-another canvas @camera/camera-position)      ;(terrain/render-to canvas w))
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (let [canvas (select root [:#canvas])
        timer (create-timer (time-ms-by-fps desired-fps) canvas)]
    ;(.start timer)
    (-> canvas
        ;(.setFocusable true)
        (request-focus!)
        (config! :paint paint-frame)
        (listen :key-pressed
                (fn [e]
                  (let [key (.getKeyCode e)]
                    (cond
                      (= key KeyEvent/VK_W)
                      (do
                        (swap! camera/target camera/move-camera-forward)
                        (swap! camera/camera-position camera/move-camera-forward))
                      (= key KeyEvent/VK_A)
                      (do
                        (swap! camera/target camera/move-camera-left)
                        (swap! camera/camera-position camera/move-camera-left))
                      (= key KeyEvent/VK_S)
                      (do
                        (swap! camera/target camera/move-camera-backward)
                        (swap! camera/camera-position camera/move-camera-backward))
                      (= key KeyEvent/VK_D)
                      (do
                        (swap! camera/target camera/move-camera-right)
                        (swap! camera/camera-position camera/move-camera-right))))
                  (.repaint canvas))))))

