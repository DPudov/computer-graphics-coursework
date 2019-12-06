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
           (java.awt.event ActionListener)
           (java.util.zip ZipEntry)))

(def desired-fps 30)

(def terrain-noise 5)

(def hue-noise 3)

(def saturation-noise 3)

(def perlin-mask 255)


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
  (- (* 0.4 dim) (* 0.02 (+ (* (- i (/ dim 2))
                               (- i (/ dim 2)))
                            (* (- j (/ dim 2))
                               (- j (/ dim 2)))))))

(defn fade
  [t]
  (* t t t (+ (* t (- (* t 6) 15)) 10)))

(defn lerp
  [t a b] (+ a (* t (- b a))))

(defn grad
  [^int hash x y z]
  (let [h (bit-and hash 15)
        u (if (< h 8) x y)
        v (if (< h 4) y
                      (if (or (= h 12)
                              (= h 14)) x z))]
    (+ (if (= (bit-and h 1) 0) x z)
       (if (= (bit-and h 2) 0) v (- v)))))

(def p (vector 151, 160, 137, 91, 90, 15,
               131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23,
               190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33,
               88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166,
               77, 146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244,
               102, 143, 54, 65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196,
               135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123,
               5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42,
               223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
               129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228,
               251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107,
               49, 192, 214, 31, 181, 199, 106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254,
               138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180))

(defn noise
  [x y z]
  (let [x-floor (int (Math/floor x))
        y-floor (int (Math/floor y))
        z-floor (int (Math/floor z))
        ^int X (bit-and x-floor perlin-mask)
        ^int Y (bit-and y-floor perlin-mask)
        ^int Z (bit-and z-floor perlin-mask)
        x-relative (- x x-floor)
        y-relative (- y y-floor)
        z-relative (- z z-floor)
        u (fade x-relative)
        v (fade y-relative)
        w (fade z-relative)
        A (+ (p X) Y)
        AA (+ (p A) Z)
        AB (+ (p (+ A 1) Z))
        B (+ (p (+ X 1)) Y)
        BA (+ (p B) Z)
        BB (+ (p (+ B 1)) Z)]
    (lerp w
          (lerp v
                (lerp u
                      (grad (p AA) x y z)
                      (grad (p BA) (- x 1) y z))
                (lerp u
                      (grad (p AB) x (- y 1) z)
                      (grad (p BB) (- x 1) (- y 1) z)))
          (lerp v
                (lerp u
                      (grad (p (+ AA 1)) x y (- z 1))
                      (grad (p (+ BA 1)) (- x 1) y (- z 1)))
                (lerp u
                      (grad (p (+ AB 1)) x (- y 1) (- z 1))
                      (grad (p (+ BB 1)) (- x 1) (- y 1) (- z 1)))))))

(defn noised-height
  [dim i j terrain-noise]
  (* 0.5 dim (+ -0.5 (noise (/ (* terrain-noise i) dim)
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
                (calculate-height (parabolic-height dim i j)
                                  (noised-height dim i j terrain-noise))))))
    terrain))

(def terrain (init-terrain 500))

(defn hsb-to-rgb
  [hue saturation brightness]
  (Color/HSBtoRGB hue saturation brightness))
;(defn color-rgb-from-hsv
;  [hue saturation value]
;  (let [hi (mod (int (/ hue 60)) 6)
;        v-min (/ (* (- 100 saturation) value) 100)
;        a (* (- value v-min) (/ (mod hue 60) 60))
;        v-inc (+ v-min a)
;        v-dec (- value a)]
;    (cond
;      (= hi 0) 2
;      (= hi 1) 1 2
;      (= hi 2) 1 2
;      (= hi 3) 1 2
;      (= hi 4) 1 2
;      (= hi 5) 1 2)))

(defn render-to
  [canvas width]
  (let [dim 500
        voxel-width (/ width dim)
        w2 (/ width 2)]
    (dotimes [i dim]
      (dotimes [j dim]
        (dotimes [k (terrain i j)]
          (let [hue (int (* 50 (noise (/ (* hue-noise i) dim)
                                      (/ (* hue-noise j) dim)
                                      (/ (* hue-noise k) dim))))
                saturation (int (+ 50 (* 100 (noise (+ 1000 (/ (* saturation-noise i) dim))
                                                    (/ (* saturation-noise j) dim)
                                                    (/ (* saturation-noise k) dim)))))
                brightness 150
                stroke-color (hsb-to-rgb hue saturation brightness)]
            (push-matrix)
            (fill stroke-color)
            (translate (+ (- w2) (* i voxel-width)))
            (pop-matrix)))))))


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
    (render-to canvas w)
    (.drawImage g canvas nil nil)))



(defn generate-world [root]
  (let [canvas (select root [:#canvas])]
    (create-timer (time-ms-by-fps desired-fps) canvas)
    (-> canvas
        (config! :paint paint-frame))))
