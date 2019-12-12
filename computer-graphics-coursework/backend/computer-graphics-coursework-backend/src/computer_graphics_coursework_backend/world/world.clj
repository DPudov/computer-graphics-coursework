(ns computer_graphics_coursework_backend.world.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.math.matrix :as matr]
            [computer_graphics_coursework_backend.render.camera :as camera]
            [computer_graphics_coursework_backend.render.drawer :as drawer]
            [computer-graphics-coursework-backend.world.terrain :as terrain]
            [computer-graphics-coursework-backend.world.water :as water]
            [computer-graphics-coursework-backend.world.perlin :as perlin])
  (:import (java.awt.image BufferedImage)
           (java.awt Color)
           (javax.swing Timer)
           (java.awt.event ActionListener KeyEvent)
           (computer_graphics_coursework_backend.math.vector Vector3D Vector4D)
           (computer_graphics_coursework_backend.math.matrix Matrix4D)
           (computer_graphics_coursework_backend.render.camera Camera)))

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

;(defn draw-cube-another
;  [canvas camera]
;  (let [mvp (camera/model-view-projection-matrix (camera/perspective (:position camera))
;                                                 ;(camera/compute-third-person-camera (:position camera) @camera/target camera/up)
;                                                 (camera/get-view-matrix camera)
;                                                 model-matrix)
;        viewport [(.getWidth canvas) (.getHeight canvas)]]
;    (terrain/render-to canvas (.getWidth canvas) mvp viewport)))

(defn create-timer
  [tick-time canvas]
  (let []
    (Timer. tick-time (reify ActionListener
                        (actionPerformed [this e]
                          ;(swap! camera/camera-position camera/rotate-camera delta-angle)
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
        canvas (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        terrain (terrain/get-terrain-voxels 50)
        water (water/get-water-voxels)
        voxels (vec (concat terrain water))]
    (clear canvas)
    (drawer/draw-voxels canvas voxels @camera/cam)
    ;(draw-cube-another canvas @camera/cam)
    ;(camera/rasterize canvas [vAf vBf vCf vDf vAb vBb vCb vDb] camera/move-camera-right);(terrain/render-to canvas w))
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
                      (swap! camera/cam camera/move-forward 1 1)
                      (= key KeyEvent/VK_A)
                      (swap! camera/cam camera/move-left 1 1)
                      (= key KeyEvent/VK_S)
                      (swap! camera/cam camera/move-backward 1 1)
                      (= key KeyEvent/VK_D)
                      (swap! camera/cam camera/move-right 1 1)
                      (or (= key KeyEvent/VK_KP_UP) (= key KeyEvent/VK_UP))
                      (swap! camera/cam camera/turn-up 1 1)
                      (or (= key KeyEvent/VK_KP_DOWN) (= key KeyEvent/VK_DOWN))
                      (swap! camera/cam camera/turn-down 1 1)
                      (or (= key KeyEvent/VK_KP_RIGHT) (= key KeyEvent/VK_RIGHT))
                      (swap! camera/cam camera/turn-right 1 1)
                      (or (= key KeyEvent/VK_KP_LEFT) (= key KeyEvent/VK_LEFT))
                      (swap! camera/cam camera/turn-left 1 1)))
                  (.repaint canvas))))))

