(ns computer_graphics_coursework_backend.world.terrain
  (:require [computer_graphics_coursework_backend.world.perlin :as perlin])
  (:import (java.awt Color)
           (computer_graphics_coursework_backend.render.voxel Voxel)))

(defn hsb-to-rgb
  [hue saturation brightness]
  (Color/HSBtoRGB hue saturation brightness))

(def terrain-noise 5)

(def hue-noise 3)

(def saturation-noise 3)

(defn parabolic-height
  [dim i j]
  (- (* 0.4 dim) (* 0.02 (+ (* (- i (/ dim 2.0))
                               (- i (/ dim 2.0)))
                            (* (- j (/ dim 2.0))
                               (- j (/ dim 2.0)))))))

(defn noised-height
  [dim i j terrain-noise]
  (* 0.5 dim (+ -0.5 (perlin/noise (/ (* terrain-noise i) (double dim))
                                   (/ (* terrain-noise j) (double dim))
                                   100.0))))

(defn calculate-height
  [parabolic noised]
  (max 0
       (int (+ parabolic noised))))

(defn init-terrain
  [dim]
  (let
    [terrain (make-array Integer/TYPE dim dim)]
    (dotimes [i dim]
      (let [^ints terrain-row (aget ^objects terrain i)]
        (dotimes [j dim]
          (if-not (or (= i 0) (= i (dec dim)) (= j 0) (= j (dec dim)))
            (do
              (aset terrain-row j
                    (int (calculate-height (parabolic-height dim i j)
                                           (noised-height dim i j terrain-noise)))))
            (aset terrain-row j
                  (int (* 1000 dim)))))))
    terrain))

(def init-terrain-memo
  (memoize init-terrain))

(defn get-terrain-voxels
  [dim]
  (let [terrain-map (init-terrain-memo dim)
        voxels (atom [])]
    (doseq [i (range 1 (dec dim))]
      (let [^ints terrain-row (aget ^objects terrain-map i)]
        (doseq [j (range 1 (dec dim))]
          (dotimes [k (aget terrain-row j)]
            (let [hue (int (+ 10 (* 50 (perlin/noise (/ (* hue-noise i) dim)
                                                     (/ (* hue-noise j) dim)
                                                     (/ (* hue-noise k) dim)))))
                  saturation (int (+ 50 (* 100 (perlin/noise (+ 1000 (/ (* saturation-noise i) dim))
                                                             (/ (* saturation-noise j) dim)
                                                             (/ (* saturation-noise k) dim)))))
                  brightness 150
                  stroke-color (Color. (hsb-to-rgb hue saturation brightness))]
              (swap! voxels conj (Voxel. i k j stroke-color)))))))
    @voxels))

(def get-terrain-voxels-memo
  (memoize get-terrain-voxels))