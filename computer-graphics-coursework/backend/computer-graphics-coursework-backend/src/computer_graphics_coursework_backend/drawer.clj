(ns computer_graphics_coursework_backend.drawer
  (:require [computer_graphics_coursework_backend.vector :as vec])
  (:import java.awt.image.BufferedImage
           (computer_graphics_coursework_backend.vector Vector3D Vector2D)
           (java.awt Color)))
(def width 640)

(def height 480)

(defn put-pixel [^BufferedImage image-buffer x y ^Color color]
  (.setRGB image-buffer x y (.getRGB color)))

(defn draw-point
  [^BufferedImage image-buffer point ^Color color]
  (let [x (int (.getX point))
        y (int (.getY point))]
    (if (and (> x 0) (< x width) (> y 0) (< y height))
      (put-pixel image-buffer x y color))))

(defn draw-line
  [^BufferedImage image-buffer xb yb xe ye ^Color color]
  (let
    [dist-x (Math/abs (- xb xe))
     dist-y (Math/abs (- yb ye))
     steep (> dist-y dist-x)
     rgb (.getRGB color)]
    (let [[xb yb xe ye] (if steep [yb xb ye xe] [xb yb xe ye])]
      (let [[xb yb xe ye] (if (> xb xe) [xe ye xb yb] [xb yb xe ye])]
        (let [delta-x (- xe xb)
              delta-y (Math/abs (- ye yb))
              y-step (if (< yb ye) 1 -1)]

          (let [plot (if steep
                       #(.setRGB image-buffer (int %1) (int %2) rgb)
                       #(.setRGB image-buffer (int %2) (int %1) rgb))]
            (loop [xi xb
                   yi yb
                   error (Math/floor (/ delta-x 2))]
              (plot xi yi)
              (if (< xi xe)
                (if (< error delta-y)
                  (recur (inc xi) (+ yi y-step) (+ error (- delta-x delta-y)))
                  (recur (inc xi) yi (- error delta-y)))))))))))

(defn draw-voxel
  [^BufferedImage image-buffer])



