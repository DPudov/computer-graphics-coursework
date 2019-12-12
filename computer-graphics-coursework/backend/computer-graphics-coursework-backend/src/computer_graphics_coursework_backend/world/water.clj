(ns computer-graphics-coursework-backend.world.water)

(def friction 0.125)

(defn init-water-prism
  [length height dim]
  (let [terrain-center (/ dim 2)
        l2 (/ length 2)
        water (make-array Integer/TYPE dim dim)]
    (for [i (range (- terrain-center l2) (+ terrain-center l2))]
      (for [j (range (- terrain-center l2) (+ terrain-center l2))]
        (aset water i j height)))))

(defn update-water
  [terrain old-water old-energy dim]
  (let [new-water (aclone old-water)
        new-energy (aclone old-energy)]
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
              front-pressure (if (< j (- dim 1) (+ (aget terrain i (+ j 1))
                                                   (aget old-water i (+ j 1))
                                                   (- (aget old-energy i (+ j 1)))) 0))]
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
          (if (and (< i (- dim 1) (> (+ (aget terrain i j) (aget old-water i j) (aget old-energy i j))) right-pressure))
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
          (if (and (< j (- dim 1) (> (+ (aget terrain i j) (aget old-water i j) (aget old-energy i j)) front-pressure)))
            (let [flow (/ (min (aget old-water i j)
                               (+ (aget terrain i j)
                                  (aget old-water i j)
                                  (aget old-energy i j)
                                  (- front-pressure)))
                          16)]
              (aset new-water i (+ j 1) (+ (aget new-water i (+ j 1)) flow))
              (aset new-water i j (+ (aget new-water i j) (- flow)))
              (aset new-energy i (+ j 1) (* (aget new-energy i (+ j 1)) (- 1 friction)))
              (aset new-energy i (+ j 1) (+ (aget new-energy i (+ j 1)) flow)))))))))




(defn init-water-energy
  [dim]
  (make-array Integer/TYPE dim dim))


(defn get-water-voxels
  [])
