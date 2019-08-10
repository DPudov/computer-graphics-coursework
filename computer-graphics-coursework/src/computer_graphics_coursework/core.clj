(ns computer-graphics-coursework.core
    (:use [clisk live]))

(def checker-pattern (checker 0 1))
(defn -main [& args]
  (show [x y 0])
  (show (scale 0.25 checker-pattern)))
  ;(show (viewport [-1 -1] [1 1])
    ;  (v*
    ;    (warp globe vplasma)      ;; colour vector plasma texture, samples on the surface of a globe
    ;    (light-value [-1 -1 1] (height-normal globe))))  ;; diffuse lighting using globe as heightmap
