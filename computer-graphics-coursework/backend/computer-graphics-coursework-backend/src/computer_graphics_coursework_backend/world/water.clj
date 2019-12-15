(ns computer_graphics_coursework_backend.world.water
  (:import (computer_graphics_coursework_backend.render.voxel Voxel)))

(def friction 0.125)

(defn init-water-prism
  [length height dim]
  (let [terrain-center (/ dim 2)
        l2 (/ length 2)
        water (make-array Double/TYPE dim dim)]
    (doseq [i (range (- terrain-center l2) (+ terrain-center l2))]
      (doseq [j (range (- terrain-center l2) (+ terrain-center l2))]
        (aset water i j height)))
    water))

(defn init-water-energy
  [dim]
  (make-array Double/TYPE dim dim))

(def l 10)
(def h 10)
(def dim 50)

(def water-map (atom (init-water-prism l h dim)))
(def energy-map (atom (init-water-energy dim)))

(defn update-water
  [terrain old-water old-energy dim]
  (let [new-water old-water
        new-energy old-energy]
    (dotimes [i dim]
      (let [^ints terrain-row-next (if (< i (dec dim)) (aget ^objects terrain (inc i)) nil)
            ^doubles water-row-next (if (< i (dec dim)) (aget ^objects old-water (inc i)) nil)
            ^doubles energy-row-next (if (< i (dec dim)) (aget ^objects old-energy (inc i)) nil)
            ^ints terrain-row-prev (if (> i 0) (aget ^objects terrain (dec i)) nil)
            ^doubles water-row-prev (if (> i 0) (aget ^objects old-water (dec i)) nil)
            ^doubles energy-row-prev (if (> i 0) (aget ^objects old-energy (dec i)) nil)
            ^ints terrain-row-cur (aget ^objects terrain i)
            ^doubles water-row-cur (aget ^objects old-water i)
            ^doubles energy-row-cur (aget ^objects old-energy i)
            ^doubles new-energy-row-prev (if (> i 0) (aget ^objects new-energy (dec i)) nil)
            ^doubles new-energy-row-cur (aget ^objects new-energy i)
            ^doubles new-energy-row-next (if (< i (dec dim)) (aget ^objects new-energy (inc i)) nil)
            ^doubles new-water-row-prev (if (> i 0) (aget ^objects new-water (dec i)) nil)
            ^doubles new-water-row-cur (aget ^objects new-water i)
            ^doubles new-water-row-next (if (< i (dec dim)) (aget ^objects new-water (inc i)) nil)]
        (dotimes [j dim]
          (let [left-pressure (if (> i 0) (+ (aget terrain-row-prev j)
                                             (aget water-row-prev j)
                                             (aget energy-row-prev j)) 0)
                right-pressure (if (< i (dec dim)) (+ (aget terrain-row-next j)
                                                      (aget water-row-next j)
                                                      (- (aget energy-row-next j))) 0)
                back-pressure (if (> j 0) (+ (aget terrain-row-cur (dec j))
                                             (aget water-row-cur (dec j))
                                             (aget energy-row-cur (dec j))) 0)
                front-pressure (if (< j (dec dim)) (+ (aget terrain-row-cur (inc j))
                                                      (aget water-row-cur (inc j))
                                                      (- (aget energy-row-cur (inc j)))) 0)]
            (if (and (> i 0) (> (+ (aget terrain-row-cur j) (aget water-row-cur j) (- (aget energy-row-cur j))) left-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (- (aget energy-row-cur j))
                                    (- left-pressure)))
                            16)]
                (aset new-water-row-prev j (+ (aget new-water-row-prev j) flow))
                (aset new-energy-row-cur j (+ (aget new-water-row-cur j) (- flow)))
                (aset new-energy-row-prev j (* (aget new-energy-row-prev j) (- 1 friction)))
                (aset new-energy-row-prev j (+ (aget new-energy-row-prev j) (- flow)))))
            (if (and (< i (dec dim)) (> (+ (aget terrain-row-cur j) (aget water-row-cur j) (aget energy-row-cur j)) right-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (aget energy-row-cur j)
                                    (- right-pressure)))
                            16)]
                (aset new-water-row-next j (+ (aget new-water-row-next j) flow))
                (aset new-water-row-cur j (+ (aget new-water-row-cur j) (+ flow)))
                (aset new-energy-row-next j (* (aget new-energy-row-next j) (- 1 friction)))
                (aset new-energy-row-next j (+ (aget new-energy-row-next j) flow))))
            (if (and (> j 0) (> (+ (aget terrain-row-cur j) (aget water-row-cur j) (- (aget energy-row-cur j))) back-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (- (aget energy-row-cur j))
                                    (- back-pressure)))
                            16)]
                (aset new-water-row-cur (dec j) (+ (aget new-water-row-cur (dec j)) flow))
                (aset new-water-row-cur j (+ (aget new-water-row-cur j) (- flow)))
                (aset new-energy-row-cur (dec j) (* (aget new-energy-row-cur (dec j)) (- 1 friction)))
                (aset new-energy-row-cur (dec j) (+ (aget new-energy-row-cur (dec j)) (- flow)))))
            (if (and (< j (dec dim)) (> (+ (aget terrain-row-cur j) (aget water-row-cur j) (aget energy-row-cur j)) front-pressure))
              (let [flow (/ (min (aget water-row-cur j)
                                 (+ (aget terrain-row-cur j)
                                    (aget water-row-cur j)
                                    (aget energy-row-cur j)
                                    (- front-pressure)))
                            16)]
                (aset new-water-row-cur (inc j) (+ (aget new-water-row-cur (inc j)) flow))
                (aset new-water-row-cur j (+ (aget new-water-row-cur j) (- flow)))
                (aset new-energy-row-cur (inc j) (* (aget new-energy-row-cur (inc j)) (- 1 friction)))
                (aset new-energy-row-cur (inc j) (+ (aget new-energy-row-cur (inc j)) flow))))))))
    [new-water new-energy]))



(defn get-water-voxels
  [dim terrain-map water-map stroke-color]
  (let [voxels (atom [])]
    (doall
      (pmap (fn [i]
              (doall (pmap (fn [j]
                             (let
                               [^ints terrain-row-cur (aget ^objects terrain-map i)
                                ^doubles water-row-cur (aget ^objects water-map i)]
                               (doseq [k (range (aget terrain-row-cur j) (+ (int (aget water-row-cur j)) (aget terrain-row-cur j)))]
                                 (swap! voxels conj (Voxel. i k j stroke-color))))) (range dim)))) (range dim)))
    @voxels))
;(dotimes [i dim]
;  (dotimes [j dim]

;@voxels ) )
