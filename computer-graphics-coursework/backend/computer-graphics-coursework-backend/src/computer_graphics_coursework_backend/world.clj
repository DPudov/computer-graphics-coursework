(ns computer_graphics_coursework_backend.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [computer_graphics_coursework_backend.vector :as vec]
            [computer_graphics_coursework_backend.matrix :as matr]
            [computer-graphics-coursework-backend.engine :as engine]
            [computer_graphics_coursework_backend.drawer :as drawer])
  (:import (computer_graphics_coursework_backend.vector Vector3D)
           (computer_graphics_coursework_backend.matrix Matrix4D)
           (java.awt.image BufferedImage)
           (java.awt Color)))


;(def l 100)

;(def camera (vec/vector 0 0 -1))
;
;(def image-width 640)
;(def image-height 480)
;(def world-to-camera (matr/matrix (vec/vector 0.707107, -0.331295, 0.624695, 0,)
;                                  (vec/vector 0, 0.883452, 0.468521, 0,)
;                                  (vec/vector -0.707107, -0.331295, 0.624695, 0,)
;                                  (vec/vector -1.63871, -5.747777, -40.400412, 1)))
;
;(def camera-to-world (matr/invert world-to-camera))

;(def inch-to-mm 25.4)
;(def k-fill 0)
;(def k-overscan 1)
;
;(def cube
;  [(vec/vector 0 0 0)
;   (vec/vector 0 0 l)
;   (vec/vector 0 l 0)
;   (vec/vector l 0 0)
;   (vec/vector l l 0)
;   (vec/vector 0 l l)
;   (vec/vector l 0 l)
;   (vec/vector l l l)])
;
;(defn get-screen-point [near ^Vector3D camera-point]
;  (let [x (.getX camera-point)
;        y (.getY camera-point)
;        z (.getZ camera-point)
;        neg-z (- z)]
;    (vec/vector (/ (* near x) neg-z)
;                (/ (* near y) neg-z)
;                neg-z)))
;
;(defn compute-screen-coordinates
;  [film-aperture-width
;   film-aperture-height
;   image-width
;   image-height
;   fit-film
;   near-clipping-plane
;   focal-length
;   top bottom left right]
;  (let [film-aspect-ratio (/ film-aperture-width film-aperture-height)
;        device-aspect-ratio (/ image-width image-height)
;        top (* near-clipping-plane (/ (/ (* film-aperture-height inch-to-mm) 2) focal-length))
;        right (* near-clipping-plane (/ (/ (* film-aperture-width inch-to-mm) 2) focal-length))
;        field-of-view (* (/ 360 Math/PI) (Math/atan (/ (/ (* film-aperture-width inch-to-mm) 2) focal-length)))
;        x-scale 1
;        y-scale 1]
;    (cond
;      (== fit-film k-fill) (if (> film-aspect-ratio device-aspect-ratio)
;                             (set! x-scale (/ device-aspect-ratio film-aspect-ratio))
;                             (set! x-scale (/ film-aspect-ratio device-aspect-ratio)))
;      (== fit-film k-overscan) (if (> film-aspect-ratio device-aspect-ratio)
;                                 (set! y-scale (/ film-aspect-ratio device-aspect-ratio))
;                                 (set! y-scale (/ device-aspect-ratio film-aspect-ratio))))
;    (println "FOV: " field-of-view)
;    (* right x-scale)
;    (* top y-scale)
;    (set! bottom (- top))
;    (set! left (- right))
;    (vec/vector top right bottom left)))


(defn paint-something [c g]
  (let [w (.getWidth c) w2 (/ w 2)
        h (.getHeight c) h2 (/ h 2)
        canvas (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
        g2d ()]
    (drawer/draw-point canvas (vec/vector 50 50) Color/BLUE)
    (drawer/draw-point canvas (vec/vector 51 50) Color/BLUE)
    (drawer/draw-point canvas (vec/vector 52 50) Color/BLUE)
    (drawer/draw-point canvas (vec/vector 53 50) Color/BLUE)
    (drawer/draw-point canvas (vec/vector 54 50) Color/BLUE)
    (drawer/draw-line canvas 10 10 200 200 Color/RED)
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (-> (select root [:#canvas])
      (config! :paint paint-something)))

;
; (defn fulfill_world
;     [])
;
; (defn update-world
;     [])
;
; (defn render-world
;     [])
