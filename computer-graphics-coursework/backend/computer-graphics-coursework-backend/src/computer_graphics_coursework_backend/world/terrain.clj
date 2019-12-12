(ns computer-graphics-coursework-backend.world.terrain
  (:require [computer-graphics-coursework-backend.world.perlin :as perlin]
            [computer_graphics_coursework_backend.render.voxel :as voxel]
            [computer_graphics_coursework_backend.render.camera :as camera])
  (:import (java.awt Color)
           (computer_graphics_coursework_backend.render.voxel Voxel)))


(defn hsb-to-rgb
  [hue saturation brightness]
  (Color/HSBtoRGB hue saturation brightness))

(def matrix-stack
  '())

(def matrix-context)

(defn push-matrix []
  (conj matrix-stack matrix-context))

(defn pop-matrix []
  (var-set matrix-context (peek matrix-stack))
  (var-set matrix-stack (pop matrix-stack)))

(def terrain-noise 5)

(def hue-noise 3)

(def saturation-noise 3)

(defn wall-level
  [dim]
  (* dim 100))


(defn parabolic-height
  [dim i j]
  (- (* 0.4 dim) (* 0.02 (+ (* (- i (/ dim 2))
                               (- i (/ dim 2)))
                            (* (- j (/ dim 2))
                               (- j (/ dim 2)))))))


(defn noised-height
  [dim i j terrain-noise]
  (* 0.5 dim (+ -0.5 (perlin/noise (/ (* terrain-noise i) dim)
                                   (/ (* terrain-noise j) dim)
                                   100))))

(defn calculate-height
  [parabolic noised]
  (max 0
       (int (+ parabolic noised))))




(defn init-terrain
  [dim]
  (let
    [terrain (make-array Integer/TYPE dim dim)]
    (dotimes [i dim]
      (dotimes [j dim]
        (if (not (or (= i 0) (= i (- dim 1) (= j 0) (= j (- dim 1)))))
          (aset terrain i j
                (int (calculate-height (parabolic-height dim i j)
                                       (noised-height dim i j terrain-noise))))
          (aset terrain i j
                (int (* 100 dim))))))
    terrain))

(def init-terrain-memo
  (memoize init-terrain))

(defn get-terrain-voxels
  [dim]
  (let [terrain-map (init-terrain-memo dim)
        voxels []]
    (dotimes [i dim]
      (dotimes [j dim]
        (dotimes [k (aget terrain-map i j)]
          (let [hue (int (* 50 (perlin/noise (/ (* hue-noise i) dim)
                                             (/ (* hue-noise j) dim)
                                             (/ (* hue-noise k) dim))))
                saturation (int (+ 50 (* 100 (perlin/noise (+ 1000 (/ (* saturation-noise i) dim))
                                                           (/ (* saturation-noise j) dim)
                                                           (/ (* saturation-noise k) dim)))))
                brightness 150
                stroke-color (Color. (hsb-to-rgb hue saturation brightness))]
            (conj voxels (Voxel. i k j stroke-color))))))))

(def get-terrain-voxels-memo
  (memoize get-terrain-voxels))

;(defn render-to
;  [canvas width mvp viewport]
;  (let [dim 10
;        voxel-width (/ width dim)
;        w2 (/ width 2)
;        terrain (init-terrain 10)]
;    (dotimes [i dim]
;      (dotimes [j dim]
;        (dotimes [k (aget terrain i j)]
;          (let [hue (int (* 50 (perlin/noise (/ (* hue-noise i) dim)
;                                             (/ (* hue-noise j) dim)
;                                             (/ (* hue-noise k) dim))))
;                saturation (int (+ 50 (* 100 (perlin/noise (+ 1000 (/ (* saturation-noise i) dim))
;                                                           (/ (* saturation-noise j) dim)
;                                                           (/ (* saturation-noise k) dim)))))
;                brightness 150
;                stroke-color (hsb-to-rgb hue saturation brightness)
;                vertices (voxel/get-vertices i j k voxel-width)]
;            (computer_graphics_coursework_backend.render.drawer/draw-voxel canvas
;                                                                           (vec (for [v vertices] (camera/project-to-screen v mvp viewport)))
;                                                                           (Color. stroke-color))))))))