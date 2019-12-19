(ns computer_graphics_coursework_backend.world.water
  (:require [computer-graphics-coursework-backend.world.config :as conf])
  (:import (computer_graphics_coursework_backend.render.voxel Voxel)))

(def ^:const friction 0.135)
(def ^:const flow-div 16.0)

(defn init-water-prism
  [length height dim]
  (let [terrain-center (/ dim 2)
        l2 (/ length 2)
        water (make-array Float/TYPE dim dim)]
    (doseq [i (range (- terrain-center l2) (+ terrain-center l2))
            :let [^floats water-row (aget ^objects water i)]]
      (doseq [j (range (- terrain-center l2) (+ terrain-center l2))]
        (aset water-row j (float height))))
    water))

(defn init-water-energy
  [dim]
  (make-array Float/TYPE dim dim))

(def l 10)
(def h 30)

(def water-map (atom (init-water-prism l h conf/dim)))
(def energy-map (atom (init-water-energy conf/dim)))

(defn print-water
  [water energy dim]
  (println "Water")
  (dotimes [i dim]
    (dotimes [j dim]
      (print (aget water i j) " "))
    (println))
  (println "Energy")
  (dotimes [i dim]
    (dotimes [j dim]
      (print (aget energy i j) " "))
    (println)))

(defn update-water
  [terrain water-map energy-map dim]
  ;(print-water old-water old-energy dim)
  (dotimes [c 10]
    (dotimes [i dim]
      (let [^ints terrain-row-next (if (< i (dec dim))
                                     (aget ^objects terrain (inc i))
                                     nil)
            ^floats water-row-next (if (< i (dec dim))
                                     (aget ^objects water-map (inc i))
                                     nil)
            ^floats energy-row-next (if (< i (dec dim))
                                      (aget ^objects energy-map (inc i))
                                      nil)
            ^ints terrain-row-prev (if (> i 0)
                                     (aget ^objects terrain (dec i))
                                     nil)
            ^floats water-row-prev (if (> i 0)
                                     (aget ^objects water-map (dec i))
                                     nil)
            ^floats energy-row-prev (if (> i 0)
                                      (aget ^objects energy-map (dec i))
                                      nil)
            ^ints terrain-row-cur (aget ^objects terrain i)
            ^floats water-row-cur (aget ^objects water-map i)
            ^floats energy-row-cur (aget ^objects energy-map i)]
        (dotimes [j dim]
          (let [left-pressure (if (> i 0)
                                (+ (aget terrain-row-prev j)
                                   (aget water-row-prev j)
                                   (aget energy-row-prev j))
                                0.0)
                right-pressure (if (< i (dec dim))
                                 (+ (aget terrain-row-next j)
                                    (aget water-row-next j)
                                    (- (aget energy-row-next j)))
                                 0.0)
                back-pressure (if (> j 0)
                                (+ (aget terrain-row-cur (dec j))
                                   (aget water-row-cur (dec j))
                                   (aget energy-row-cur (dec j)))
                                0.0)
                front-pressure (if (< j (dec dim))
                                 (+ (aget terrain-row-cur (inc j))
                                    (aget water-row-cur (inc j))
                                    (- (aget energy-row-cur (inc j))))
                                 0.0)]
            (if (and (> i 0)
                     (> (+ (aget terrain-row-cur j)
                           (aget water-row-cur j)
                           (aget energy-row-cur j))
                        left-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (- (aget energy-row-cur j))
                                    (- left-pressure)))
                            flow-div)]
                (aset water-row-prev j (+ (aget water-row-prev j) flow))
                (aset water-row-cur j (+ (aget water-row-cur j) (- flow)))
                (aset energy-row-prev j (* (aget energy-row-prev j) (- 1.0 friction)))
                (aset energy-row-prev j (+ (aget energy-row-prev j) (- flow)))))
            (if (and (< i (dec dim))
                     (> (+ (aget terrain-row-cur j)
                           (aget water-row-cur j)
                           (aget energy-row-cur j))
                        right-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (aget energy-row-cur j)
                                    (- right-pressure)))
                            flow-div)]
                (aset water-row-next j (+ (aget water-row-next j) flow))
                (aset water-row-cur j (+ (aget water-row-cur j) (- flow)))
                (aset energy-row-next j (* (aget energy-row-next j) (- 1.0 friction)))
                (aset energy-row-next j (+ (aget energy-row-next j) flow))))
            (if (and (> j 0)
                     (> (+ (aget terrain-row-cur j)
                           (aget water-row-cur j)
                           (- (aget energy-row-cur j)))
                        back-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (- (aget energy-row-cur j))
                                    (- back-pressure)))
                            flow-div)]
                (aset water-row-cur (dec j) (+ (aget water-row-cur (dec j)) flow))
                (aset water-row-cur j (+ (aget water-row-cur j) (- flow)))
                (aset energy-row-cur (dec j) (* (aget energy-row-cur (dec j)) (- 1.0 friction)))
                (aset energy-row-cur (dec j) (+ (aget energy-row-cur (dec j)) (- flow)))))
            (if (and (< j (dec dim))
                     (> (+ (aget terrain-row-cur j)
                           (aget water-row-cur j)
                           (aget energy-row-cur j))
                        front-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (aget energy-row-cur j)
                                    (- front-pressure)))
                            flow-div)]
                (aset water-row-cur (inc j) (+ (aget water-row-cur (inc j)) flow))
                (aset water-row-cur j (+ (aget water-row-cur j) (- flow)))
                (aset energy-row-cur (inc j) (* (aget energy-row-cur (inc j)) (- 1.0 friction)))
                (aset energy-row-cur (inc j) (+ (aget energy-row-cur (inc j)) flow)))))))))
  [water-map energy-map])


(defn get-water-voxels
  [dim terrain-map water-map stroke-color]

  (flatten (map (fn [i]
                  (let
                    [^ints terrain-row-cur (aget ^objects terrain-map i)
                     ^floats water-row-cur (aget ^objects water-map i)]
                    (flatten (map (fn [j]
                                    (map (fn [k]
                                           (Voxel. i k j stroke-color))

                                         (range (aget terrain-row-cur j) (+ (int (Math/round ^float (aget water-row-cur j)))
                                                                            (aget terrain-row-cur j)))))
                                  (range dim)))))
                (range dim))))

