(ns computer_graphics_coursework_backend.render.camera
  (:require [computer_graphics_coursework_backend.math.matrix :as matr]
            [computer_graphics_coursework_backend.math.vector :as vec])

  (:import (computer_graphics_coursework_backend.math.vector Vector4D Vector3D)
           (java.awt.image BufferedImage)
           (java.awt Color)))
(def camera-position (atom (Vector4D. 0 3 -10 1)))
(def camera-orientation (Vector4D. 0 1 0 1))
(def eye (Vector4D. 0 0 5 1))
(def target (atom (Vector4D. 0 0 2 1)))
(def up (Vector4D. 0 1 0 1))
(def fovy 0.785)
(def aspect-ratio 4/3)
(def near 1)
(def far 80)
;
;(defn projection-matrix
;  [camera-position fovy aspect-ratio near far]
;  (let [c (if (= (camera-position 3) 0) 0 (/ 1 (camera-position 3)))]
;    (matr/mat4 1 0 0 0
;               0 1 0 0
;               0 0 1 (- c)
;               0 0 0 1)))

(defn projection-matrix
  [camera-position fovy aspect-ratio near far]
  (let [top (* (Math/tan (/ fovy 2)) near)
        bottom (- top)
        right (* top aspect-ratio)
        left (- right)]

    (matr/mat4 (/ (* near 2) (- right left)) 0 (/ (+ right left) (- right left)) 0
               0 (/ (* near 2) (- top bottom)) (/ (+ top bottom) (- top bottom)) 0
               0 0 (- (/ (+ far near) (- far near))) (- (/ (* 2 far near) (- far near)))
               0 0 -1 0)))

(defn model-view-projection-matrix
  [projection view model]
  (matr/mult projection view model))

(defn perspective
  [position]
  (projection-matrix position
                     fovy
                     aspect-ratio
                     near
                     far))

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
  (let [screen-point (perspective-divide (matr/transform model-view-projection point))]
    (to-screen screen-point view-port)))

(defprotocol Viewable
  (get-view-matrix [this]))

(defrecord Camera
  [position yaw pitch roll]
  Viewable
  (get-view-matrix [this]
    (let [roll-matrix (matr/rotate-z (- roll))
          pitch-matrix (matr/rotate-x (- pitch))
          yaw-matrix (matr/rotate-y (- yaw))
          translation (matr/translate-mat4 [(- (position 0))
                                            (- (position 1))
                                            (- (position 2))])]
      (matr/mult roll-matrix pitch-matrix yaw-matrix translation))))

(defn move-forward
  [camera speed delta-time]
  (let [position (:position camera)
        yaw (:yaw camera)
        distance (* speed delta-time)
        position-x (+ (position 0) (* distance (Math/sin yaw)))
        position-z (+ (position 2) (* distance (Math/cos yaw)))
        position-upd (Vector4D. position-x (position 1) position-z 1)]
    (assoc camera :position position-upd)))


(defn move-backward
  [camera speed delta-time]
  (let [position (:position camera)
        yaw (:yaw camera)
        distance (* speed delta-time)
        position-x (- (position 0) (* distance (Math/sin yaw)))
        position-z (- (position 2) (* distance (Math/cos yaw)))
        position-upd (Vector4D. position-x (position 1) position-z 1)]
    (assoc camera :position position-upd)))


(defn move-right
  [camera speed delta-time]
  (let [position (:position camera)
        yaw (:yaw camera)
        distance (* speed delta-time)
        position-x (- (position 0) (* distance (Math/cos yaw)))
        position-z (- (position 2) (* distance (Math/sin yaw)))
        position-upd (Vector4D. position-x (position 1) position-z 1)]
    (assoc camera :position position-upd)))


(defn move-left
  [camera speed delta-time]
  (let [position (:position camera)
        yaw (:yaw camera)
        distance (* speed delta-time)
        position-x (+ (position 0) (* distance (Math/cos yaw)))
        position-z (+ (position 2) (* distance (Math/sin yaw)))
        position-upd (Vector4D. position-x (position 1) position-z 1)]
    (assoc camera :position position-upd)))

(defn turn-left
  [camera speed delta-time]
  (let [distance (* speed delta-time)
        yaw-upd (+ (:yaw camera) distance)]
    (assoc camera :yaw yaw-upd)))


(defn turn-right
  [camera speed delta-time]
  (let [distance (* speed delta-time)
        yaw-upd (- (:yaw camera) distance)]
    (assoc camera :yaw yaw-upd)))

(defn turn-up
  [camera speed delta-time]
  (let [distance (* speed delta-time)
        pitch-upd (+ (:pitch camera) distance)]
    (assoc camera :pitch pitch-upd)))


(defn turn-down
  [camera speed delta-time]
  (let [distance (* speed delta-time)
        pitch-upd (- (:pitch camera) distance)]
    (assoc camera :pitch pitch-upd)))

(defn compute-third-person-camera
  [eye target up]
  (let [translation (matr/translate-mat4 [(- (eye 0))
                                          (- (eye 1))
                                          (- (eye 2))])
        forward-vec (vec/normalize (vec/sub-4d eye target))
        left-vec (vec/normalize (vec/cross up forward-vec))
        up-vec (vec/cross forward-vec left-vec)
        rotation-matrix (matr/mat4 (left-vec 0) (left-vec 1) (left-vec 2) 0
                                   (up-vec 0) (up-vec 1) (up-vec 2) 0
                                   (forward-vec 0) (forward-vec 1) (forward-vec 2) 0
                                   0 0 0 1)]
    (matr/mult rotation-matrix translation)))

(def cam (atom (Camera. @camera-position 0 0 0)))
