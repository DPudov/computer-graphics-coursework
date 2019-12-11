(ns computer_graphics_coursework_backend.render.camera
  (:require [computer_graphics_coursework_backend.math.matrix :as matr]
            [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.render.drawer :as drawer])

  (:import (computer_graphics_coursework_backend.math.vector Vector4D Vector3D)
           (java.awt.image BufferedImage)
           (java.awt Color)))

(def camera-position (atom (Vector4D. 0 3 -10 1)))
(def camera-orientation (Vector4D. 0 1 0 1))
(def camera-target (atom (Vector4D. 0 0 0 1)))
(def display-surface (Vector4D. 0 0 1 1))

(def eye (Vector4D. 0 0 5 1))
(def target (atom (Vector4D. 0 0 2 1)))
(def up (Vector4D. 0 1 0 1))
(def fovy 0.785)
(def aspect-ratio 4/3)
(def near 1)
(def far 80)
(defn projection-matrix
  [camera-position fovy aspect-ratio near far]
  (let [c (if (= (camera-position 3) 0) 0 (/ 1 (camera-position 3)))]
    (matr/mat4 1 0 0 0
               0 1 0 0
               0 0 1 (- c)
               0 0 0 1)))
;(let [top 3                                               ;(* (Math/tan (/ fovy 2)) near)
;      bottom -3                                           ;(- top)
;      right 4                                             ; (* top aspect-ratio)
;      left -4]                                            ;(- right)]
;
;  (matr/mat4 (/ (* near 2) (- right left)) 0 (/ (+ right left) (- right left)) 0
;             0 (/ (* near 2) (- top bottom)) (/ (+ top bottom) (- top bottom)) 0
;             0 0 (- (/ (+ far near) (- far near))) (- (/ (* 2 far near) (- far near)))
;             0 0 -1 0)))

(defn view-matrix
  [eye target up]
  (let
    [z-axis (vec/normalize (vec/sub eye target))
     x-axis (vec/normalize (vec/cross up z-axis))
     y-axis (vec/cross z-axis x-axis)]
    (matr/mat4 (x-axis 0) (y-axis 0) (z-axis 0) 0
               (x-axis 1) (y-axis 1) (z-axis 1) 0
               (x-axis 2) (y-axis 2) (z-axis 2) 0
               ;0 0 0 1)))
               (- (vec/dot x-axis eye)) (- (vec/dot y-axis eye)) (- (vec/dot z-axis eye)) 1)))
; translation (matr/mat4 1 0 0 0
;                        0 1 0 0
;                        0 0 1 0
;                        (- (eye 0)) (- (eye 1)) (- (eye 2)) 1)]
;(matr/mult orientation translation)))

(def perspective
  (projection-matrix @camera-position
                     fovy
                     aspect-ratio
                     near
                     far))

(def view
  (view-matrix eye @target up))

(defn model-view-projection-matrix
  [projection view model]
  (matr/mult projection view model))

(defn rotate-camera-ox
  [camera-position angle]
  (matr/transform (matr/mult (matr/rotate-x angle))
                  camera-position))


(defn rotate-camera-oy
  [camera-position angle]
  (matr/transform (matr/mult (matr/rotate-y angle))
                  camera-position))


(defn rotate-camera-oz
  [camera-position angle]
  (matr/transform (matr/mult (matr/rotate-z angle))
                  camera-position))

(defn rotate-camera
  [camera-position angle]
  (let [theta-x (angle 0)
        theta-y (angle 1)
        theta-z (angle 2)]
    (matr/transform (matr/mult (matr/translate-mat4 (vec/sub @target camera-position))
                               (matr/rotate-x theta-x)
                               (matr/rotate-y theta-y)
                               (matr/rotate-z theta-z))
                    ;(matr/translate-mat4 (vec/sub camera-position @target)))
                    camera-position)))
(defn apply-func-to-vec
  [vec func & args]
  (reduce (fn [r [k v]] (assoc vec (apply func r args)) vec)))

(defn perspective-divide
  [point]
  (let [
        z (point 2)
        z-neg (- z)
        x (point 0)
        y (point 1)]
    (Vector4D. (/ x z-neg)
               (/ y z-neg)
               z
               1)))

(defn check-boundaries
  [point canvas-width canvas-height]
  (let [x (point 0)
        y (point 1)]
    (and (> x 0) (> y 0) (< x canvas-width) (< y canvas-height))))


(defn convert-point-to-raster-from-ndc
  [point width height]
  (let [x (point 0)
        y (point 1)
        z (point 2)
        w (point 3)]
    (Vector4D. (* x width)
               (* y height)
               z
               w)))

(defn convert-point-to-ndc-from-screen
  [point]
  (let [x (point 0)
        y (point 1)
        z (point 2)
        w (point 3)]
    (Vector4D. (/ (+ x 1) 2)
               (/ (+ y 1) 2)
               (/ (+ z 1) 2)
               w)))

(defn convert-point-to-raster-from-screen
  [point width height]
  (if (check-boundaries point width height)
    (convert-point-to-raster-from-ndc (convert-point-to-ndc-from-screen point) width height)
    (Vector4D. 0 0 0 1)))


(defn rasterize
  [^BufferedImage image scene-vertices transformation & args]
  (let [points-in-camera-space (apply-func-to-vec scene-vertices transformation)
        points-perspective-divided (apply-func-to-vec points-in-camera-space perspective-divide)
        points-raster (apply-func-to-vec points-perspective-divided convert-point-to-raster-from-screen)]
    (doseq [el points-raster] (drawer/draw-point image el Color/BLUE))))

(defn translate-camera
  [camera-position move]
  (matr/transform (matr/translate-mat4 move) camera-position))

(defn move-camera-forward
  [camera-position]
  (translate-camera camera-position [0 0 1]))

(defn move-camera-backward
  [camera-position]
  (translate-camera camera-position [0 0 -1]))

(defn move-camera-right
  [camera-position]
  (translate-camera camera-position [1 0 0]))

(defn move-camera-left
  [camera-position]
  (translate-camera camera-position [-1 0 0]))

(defn perspective-divide
  [v]
  (let [w (v 3)
        x (if (= w 0.0) 0 (/ (v 0) w))
        y (if (= w 0.0) 0 (/ (v 1) w))
        z (if (= w 0.0) 0 (/ (v 2) w))]
    (Vector4D. x y z w)))

(defn to-screen
  [v viewport]
  (Vector3D. (+ (/ (* (viewport 0) (v 0)) 2) (/ (viewport 0) 2))
             (/ (* (viewport 1) (+ (v 1) 1)) 2)
             (/ (+ (v 2) 1) 2)))

(defn project-to-screen
  [point model-view-projection view-port]
  ;(perspective-divide (matr/transform model-view-projection point)))
  ;(matr/transform model-view-projection point)
  (let [screen-point (perspective-divide (matr/transform model-view-projection point))]
    (to-screen screen-point view-port)))
