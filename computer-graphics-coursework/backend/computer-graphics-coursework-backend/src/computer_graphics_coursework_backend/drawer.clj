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

(defn draw-line [])