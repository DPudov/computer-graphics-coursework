(ns computer-graphics-coursework-backend.render.color
  (:import (java.awt Color)))
;
;(defprotocol MathColor
;  (lerp [this other t])
;  (sub [this other])
;  (add [this other]))
;
;(defrecord Color
;  [red green blue alpha]
;  MathColor
;  (lerp [this other t])
;  (sub [this t])
;  (add [this other] (update-in this :red (+ (:red this) (:red other))
;                               this :green (+ (:green this) (:green other))
;                               this :blue (+ :green t))))

(defn gray-color
  [x]
  (Color. x x x 1))

(defn sum-colors
  [c1 c2]
  (let
    [c1-int (.getRGB c1)
     c2-int (.getRGB c2)]
    (Color. (+ c1-int c2-int) true)))

(defn scale-color
  [c v]
  (let [c-int (.getRGB c)]
    (Color. (int (* c-int v)) true)))

