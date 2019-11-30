(ns computer_graphics_coursework_backend.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color))
(def l 100)

(def camera [0 0 -1])

(def cube
  [[0 0 0]
   [l 0 0]
   [0 l 0]
   [0 0 l]
   [l l 0]
   [l 0 l]
   [0 l l]
   [l l l]])

(defn get-screen-point [near camera-point])

(defn projection [figure]
  (apply vertices))


(defn paint-something [c g]
  (let [w (.getWidth c) w2 (/ w 2)
        h (.getHeight c) h2 (/ h 2)]
    (draw g)))

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
