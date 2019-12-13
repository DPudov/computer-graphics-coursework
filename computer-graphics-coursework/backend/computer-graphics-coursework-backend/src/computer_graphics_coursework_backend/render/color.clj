(ns computer_graphics_coursework_backend.render.color
  (:import (java.awt Color)))

(defrecord MyColor
  [r g b a])

(defn from-java-color
  [java-color]
  (MyColor. (.getRed java-color)
            (.getGreen java-color)
            (.getBlue java-color)
            (.getAlpha java-color)))

(defn to-java-color
  [my-color]
  (Color. (:r my-color)
          (:g my-color)
          (:b my-color)
          (:a my-color)))

(defn sum-colors
  [c1 c2]
  (let [r1 (.getRed c1)
        g1 (.getGreen c1)
        b1 (.getBlue c1)
        a1 (.getAlpha c1)
        r2 (.getRed c2)
        g2 (.getGreen c2)
        b2 (.getBlue c2)
        a2 (.getAlpha c2)]
    (Color. (+ r1 r2)
            (+ g1 g2)
            (+ b1 b2)
            (+ a1 a2))))

(defn sub-colors
  [c1 c2]
  (let [r1 (.getRed c1)
        g1 (.getGreen c1)
        b1 (.getBlue c1)
        a1 (.getAlpha c1)
        r2 (.getRed c2)
        g2 (.getGreen c2)
        b2 (.getBlue c2)
        a2 (.getAlpha c2)]
    (Color. (- r1 r2)
            (- g1 g2)
            (- b1 b2)
            (- a1 a2))))

(defn scale-color
  [color coef]
  (let [new-red (* coef (.getRed color))
        new-green (* coef (.getGreen color))
        new-blue (* coef (.getBlue color))]))