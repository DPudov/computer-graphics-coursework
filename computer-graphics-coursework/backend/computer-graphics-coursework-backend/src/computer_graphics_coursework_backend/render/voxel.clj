(ns computer_graphics_coursework_backend.render.voxel
  (:import (computer_graphics_coursework_backend.math.vector Vector4D)))

(defrecord Voxel
  [x y z color])

(defrecord VoxelNormal
  [axis sign])

(defrecord VoxelPlane
  [normal position color])

(defrecord VoxelFace
  [i0 j0 i1 j1])

(def voxel-x 1)
(def voxel-y 2)
(def voxel-z 3)

(def voxel-pos-x (VoxelNormal. voxel-x 1))
(def voxel-neg-x (VoxelNormal. voxel-x -1))
(def voxel-pos-y (VoxelNormal. voxel-y 1))
(def voxel-neg-y (VoxelNormal. voxel-y -1))
(def voxel-pos-z (VoxelNormal. voxel-z 1))
(def voxel-neg-z (VoxelNormal. voxel-z -1))

(def voxel-base-count (atom 256))

(defn fill-lookup-table
  [voxels]
  (let [count @voxel-base-count
        lookup-table (make-array Boolean/TYPE count count count)]
    (doseq [voxel voxels
            :let [x (:x voxel)
                  y (:y voxel)
                  z (:z voxel)]]
      (aset lookup-table x y z true))))

(defn combine-voxel-faces [faces]
  (let [i0 (min-key :i0 faces)
        i1 (max-key :i1 faces)
        j0 (min-key :j0 faces)
        j1 (max-key :j1 faces)
        nj (+ (- j1 j0) 1)
        ni (+ (- i1 i0) 1)
        a (make-array Integer/TYPE nj ni)
        w (make-array Integer/TYPE nj ni)
        h (make-array Integer/TYPE nj ni)
        c (atom 0)
        result (list)]
    (for [f faces]
      (for [j (range (:j0 f) (+ (:j1 f) 1))]
        (for [i (range (:i0 f) (+ (:i0 f) 1))]
          (do
            (aset a (- j j0) (- i i0) 1)
            (swap! c inc)))))
    (for [k (range @c)
          :let [maxArea 0
                maxFace (VoxelFace. i0 j0 i1 j1)]]
      (for [j (range nj)]
        (for [i (range ni)
              :let [el (aget a j i)]]
          ()))
      (conj result maxFace))))

(defn generate-voxel-mesh
  [voxels]
  (let
    [lookup-table (fill-lookup-table voxels)
     plane-faces (list)
     lines (list)
     triangles (list)]
    (doseq [voxel voxels
            :let [x (:x voxel)
                  y (:y voxel)
                  z (:z voxel)
                  c (:color voxel)
                  e1 (aget lookup-table (+ x 1) y z)
                  e2 (aget lookup-table (- x 1) y z)
                  e3 (aget lookup-table x (+ y 1) z)
                  e4 (aget lookup-table x (- y 1) z)
                  e5 (aget lookup-table x y (+ z 1))
                  e6 (aget lookup-table x y (- z 1))
                  p1 (VoxelPlane. voxel-pos-x x c)
                  f1 (VoxelFace. y z y z)
                  p2 (VoxelPlane. voxel-neg-x x c)
                  f2 (VoxelFace. y z y z)
                  p3 (VoxelPlane. voxel-pos-y y c)
                  f3 (VoxelFace. x z x z)
                  p4 (VoxelPlane. voxel-neg-y y c)
                  f4 (VoxelFace. x z x z)
                  p5 (VoxelPlane. voxel-pos-z z c)
                  f5 (VoxelFace. x y x y)
                  p6 (VoxelPlane. voxel-neg-z z c)
                  f6 (VoxelFace. x y x y)]]
      (if e1 (conj plane-faces [p1 f1]))
      (if e2 (conj plane-faces [p2 f2]))
      (if e3 (conj plane-faces [p3 f3]))
      (if e4 (conj plane-faces [p4 f4]))
      (if e5 (conj plane-faces [p5 f5]))
      (if e6 (conj plane-faces [p6 f6])))
    (doseq [[plane faces] (to-nested plane-faces)
            :let [f (combine-voxel-faces faces)]]
      (concat lines (outline-voxel-faces plane f))
      (concat triangles (triangulate-voxel-faces plane f)))
    (mesh triangles lines)))

;
;(defn get-vertices
;  [x y z voxel-width]
;  (let [w2 (/ voxel-width 2)
;        vafp (Vector4D. (- x w2) (+ y w2) (+ z w2) 1)
;        vbfp (Vector4D. (+ x w2) (+ y w2) (+ z w2) 1)
;        vcfp (Vector4D. (+ x w2) (- y w2) (+ z w2) 1)
;        vdfp (Vector4D. (- x w2) (- y w2) (+ z w2) 1)
;        vabp (Vector4D. (- x w2) (+ y w2) (- z w2) 1)
;        vbbp (Vector4D. (+ x w2) (+ y w2) (- z w2) 1)
;        vcbp (Vector4D. (+ x w2) (- y w2) (- z w2) 1)
;        vdbp (Vector4D. (- x w2) (- y w2) (- z w2) 1)]
;    [vafp vbfp vcfp vdfp vabp vbbp vcbp vdbp]))
