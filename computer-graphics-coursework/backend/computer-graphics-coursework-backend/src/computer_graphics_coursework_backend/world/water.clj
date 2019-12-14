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
      (dotimes [j dim]
        (let [left-pressure (if (> i 0) (+ (aget terrain (dec i) j)
                                           (aget old-water (dec i) j)
                                           (aget old-energy (dec i) j)) 0)
              right-pressure (if (< i (dec dim)) (+ (aget terrain (inc i) j)
                                                    (aget old-water (inc i) j)
                                                    (- (aget old-energy (inc i) j))) 0)
              back-pressure (if (> j 0) (+ (aget terrain i (dec j))
                                           (aget old-water i (dec j))
                                           (aget old-energy i (dec j))) 0)
              front-pressure (if (< j (dec dim)) (+ (aget terrain i (inc j))
                                                    (aget old-water i (inc j))
                                                    (- (aget old-energy i (inc j)))) 0)]
          (if (and (> i 0) (> (+ (aget terrain i j) (aget old-water i j) (- (aget old-energy i j))) left-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (- (aget old-energy i j))
                                  (- left-pressure)))
                          16)]
              (aset new-water (dec i) j (+ (aget new-water (dec i) j) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy (dec i) j (* (aget new-energy (dec i) j) (- 1 friction)))
              (aset new-energy (dec i) j (+ (aget new-energy (dec i) j) (- flow)))))
          (if (and (< i (dec dim)) (> (+ (aget terrain i j) (aget old-water i j) (aget old-energy i j)) right-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (aget old-energy i j)
                                  (- right-pressure)))
                          16)]
              (aset new-water (inc i) j (+ (aget new-water (inc i) j) flow))
              (aset new-water i j (+ (aget new-water i j) (+ flow)))
              (aset new-energy (inc i) j (* (aget new-energy (inc i) j) (- 1 friction)))
              (aset new-energy (inc i) j (+ (aget new-energy (inc i) j) flow))))
          (if (and (> j 0) (> (+ (aget terrain i j) (aget old-water i j) (- (aget old-energy i j))) back-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (- (aget old-energy i j))
                                  (- back-pressure)))
                          16)]
              (aset new-water i (dec j) (+ (aget new-water i (dec j)) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy i (dec j) (* (aget new-energy i (dec j)) (- 1 friction)))
              (aset new-energy i (dec j) (+ (aget new-energy i (dec j)) (- flow)))))
          (if (and (< j (dec dim)) (> (+ (aget terrain i j) (aget old-water i j) (aget old-energy i j)) front-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (aget old-energy i j)
                                  (- front-pressure)))
                          16)]
              (aset new-water i (inc j) (+ (aget new-water i (inc j)) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy i (inc j) (* (aget new-energy i (inc j)) (- 1 friction)))
              (aset new-energy i (inc j) (+ (aget new-energy i (inc j)) flow)))))))
    [new-water new-energy]))



(defn get-water-voxels
  [dim terrain-map water-map stroke-color]
  (let [voxels (atom [])]
    (doall
      (pmap (fn [i]
              (doall (pmap (fn [j]
                             (doseq [k (range (aget terrain-map i j) (+ (int (aget water-map i j)) (aget terrain-map i j)))]
                               (swap! voxels conj (Voxel. i k j stroke-color)))) (range dim)))) (range dim)))
    @voxels))
;(dotimes [i dim]
;  (dotimes [j dim]

;@voxels ) )
