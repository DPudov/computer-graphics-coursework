(ns computer-graphics-coursework-backend.world.terrain
  (:require [computer-graphics-coursework-backend.world.perlin :as perlin])
  (:import (java.awt Color)))


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

(defn render-to
  [canvas width]
  (let [dim 500
        voxel-width (/ width dim)
        w2 (/ width 2)
        terrain (init-terrain 500)]
    (dotimes [i dim]
      (dotimes [j dim]
        (println terrain)
        (dotimes [k (aget terrain i j)]
          (let [hue (int (* 50 (perlin/noise (/ (* hue-noise i) dim)
                                             (/ (* hue-noise j) dim)
                                             (/ (* hue-noise k) dim))))
                saturation (int (+ 50 (* 100 (perlin/noise (+ 1000 (/ (* saturation-noise i) dim))
                                                           (/ (* saturation-noise j) dim)
                                                           (/ (* saturation-noise k) dim)))))
                brightness 150
                stroke-color (hsb-to-rgb hue saturation brightness)]
            (push-matrix)

            (pop-matrix)))))))