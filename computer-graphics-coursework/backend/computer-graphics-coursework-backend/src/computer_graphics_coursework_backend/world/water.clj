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
(def h 30)
(def dim 50)

(def water-map (atom (init-water-prism l h dim)))
(def energy-map (atom (init-water-energy dim)))

(defn update-water
  [terrain old-water old-energy dim]
  (let [new-water old-water
        new-energy old-energy]
    (dotimes [i dim]
      (dotimes [j dim]
        (let [left-pressure (if (> i 0) (+ (aget terrain (- i 1) j)
                                           (aget old-water (- i 1) j)
                                           (aget old-energy (- i 1) j)) 0)
              right-pressure (if (< i (- dim 1)) (+ (aget terrain (+ i 1) j)
                                                    (aget old-water (+ i 1) j)
                                                    (- (aget old-energy (+ i 1) j))) 0)
              back-pressure (if (> j 0) (+ (aget terrain i (- j 1))
                                           (aget old-water i (- j 1))
                                           (aget old-energy i (- j 1))) 0)
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
              (aset new-water (- i 1) j (+ (aget new-water (- i 1) j) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy (- i 1) j (* (aget new-energy (- i 1) j) (- 1 friction)))
              (aset new-energy (- i 1) j (+ (aget new-energy (- i 1) j) (- flow)))))
          (if (and (< i (- dim 1)) (> (+ (aget terrain i j) (aget old-water i j) (aget old-energy i j)) right-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (aget old-energy i j)
                                  (- right-pressure)))
                          16)]
              (aset new-water (+ i 1) j (+ (aget new-water (+ i 1) j) flow))
              (aset new-water i j (+ (aget new-water i j) (+ flow)))
              (aset new-energy (+ i 1) j (* (aget new-energy (+ i 1) j) (- 1 friction)))
              (aset new-energy (+ i 1) j (+ (aget new-energy (+ i 1) j) flow))))
          (if (and (> j 0) (> (+ (aget terrain i j) (aget old-water i j) (- (aget old-energy i j))) back-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (- (aget old-energy i j))
                                  (- back-pressure)))
                          16)]
              (aset new-water i (- j 1) (+ (aget new-water i (- j 1)) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy i (- j 1) (* (aget new-energy i (- j 1)) (- 1 friction)))
              (aset new-energy i (- j 1) (+ (aget new-energy i (- j 1)) (- flow)))))
          (if (and (< j (- dim 1)) (> (+ (aget terrain i j) (aget old-water i j) (aget old-energy i j)) front-pressure))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (aget old-energy i j)
                                  (- front-pressure)))
                          16)]
              (aset new-water i (+ j 1) (+ (aget new-water i (+ j 1)) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy i (+ j 1) (* (aget new-energy i (+ j 1)) (- 1 friction)))
              (aset new-energy i (+ j 1) (+ (aget new-energy i (+ j 1)) flow)))))))
    [new-water new-energy]))



(defn get-water-voxels
  [dim terrain-map water-map stroke-color]
  (let [voxels (atom [])]
    (dotimes [i dim]
      (dotimes [j dim]
        (doseq [k (range (aget terrain-map i j) (+ (int (aget water-map i j)) (aget terrain-map i j)))]
          (swap! voxels conj (Voxel. i k j stroke-color)))))
    @voxels))
