(ns computer_graphics_coursework_backend.render.voxel
  (:require [computer_graphics_coursework_backend.render.mesh])
  (:import (computer_graphics_coursework_backend.math.vector Vector4D)
           (computer_graphics_coursework_backend.render.mesh Mesh)
           (java.util HashMap)))

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
  (->> voxels
       (pmap (fn [voxel] [(:x voxel) (:y voxel) (:z voxel)]))
       (into [])))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))


(defn find-exposed-faces
  [voxels lookup-table]
  (let [m (HashMap.)]
    (doseq [voxel voxels
            :let [x (:x voxel)
                  y (:y voxel)
                  z (:z voxel)
                  c (:color voxel)]]
      (do
        (if (in? lookup-table [(inc x) y z])
          (let [plane (VoxelPlane. voxel-pos-x x c)
                face (VoxelFace. y z y z)]
            (.put m plane (conj (.get m plane) face))))
        (if (in? lookup-table [(dec x) y z])
          (let [plane (VoxelPlane. voxel-neg-x x c)
                face (VoxelFace. y z y z)]
            (.put m plane (conj (.get m plane) face))))
        (if (in? lookup-table [x (inc y) z])
          (let [plane (VoxelPlane. voxel-pos-y y c)
                face (VoxelFace. x z x z)]
            (.put m plane (conj (.get m plane) face))))
        (if (in? lookup-table [x (dec y) z])
          (let [plane (VoxelPlane. voxel-neg-y y c)
                face (VoxelFace. x z x z)]
            (.put m plane (conj (.get m plane) face))))
        (if (in? lookup-table [x y (inc z)])
          (let [plane (VoxelPlane. voxel-pos-z z c)
                face (VoxelFace. x y x y)]
            (.put m plane (conj (.get m plane) face))))
        (if (in? lookup-table [x y (dec z)])
          (let [plane (VoxelPlane. voxel-neg-z z c)
                face (VoxelFace. x y x y)]
            (.put m plane (conj (.get m plane) face))))))
    m))

(defn calculate-max-face [i0 j0 nj ni a h w]
  (let [max-face (atom (VoxelFace. 0 0 0 0))
        max-area (atom 0)
        minw (atom 0)]
    (doseq [j (range nj)]
      (doseq [i (range ni)
              :when (not (zero? (aget a j i)))]
        (if (zero? j)
          (aset h j i 1)
          (aset h j i (inc (aget h (dec j) i))))
        (if (zero? i)
          (aset w j i 1)
          (aset w j i (inc (aget w j (dec i)))))
        (reset! minw (aget w j i))
        (doseq [dh (range (aget h j i))]
          (if (< (aget w (- j dh) i) @minw)
            (reset! minw (aget w (- j dh i))))
          (let [area (* (inc dh) @minw)]
            (if (> area @max-area)
              (do
                (reset! max-area area)
                (reset! max-face (VoxelFace.
                                   (inc (+ i0 i (- @minw)))
                                   (+ j0 j (- dh))
                                   (+ i i0)
                                   (+ j j0)))))))))
    @max-face))


(defn combine-faces-single-plane
  "Find large rectangles"
  [plane-faces]
  (let [faces (.getValue plane-faces)
        i0-bound (apply min-key :i0 faces)
        j0-bound (apply min-key :j0 faces)
        i1-bound (apply min-key :i1 faces)
        j1-bound (apply min-key :j1 faces)
        nj (inc (- j1-bound j0-bound))
        ni (inc (- i1-bound i0-bound))
        a (make-array Integer/TYPE nj ni)
        w (make-array Integer/TYPE nj ni)
        h (make-array Integer/TYPE nj ni)
        counter (atom 0)]
    (doseq [f faces]
      (doseq [j (range (:j0 f) (inc (:j1 f)))]
        (doseq [i (range ((:i0 f) (inc (:i1 f))))]
          (aset a (- j j0-bound) (- i i0-bound) 1)
          (swap! counter inc))))
    (loop [counter @counter result (transient [])]
      (if (< 0 @counter)
        (let [max-face (calculate-max-face i0-bound j0-bound nj ni a h w)]
          (doseq [j (range (:j0 max-face) (inc (:j1 max-face)))]
            (doseq [i (range (:i0 max-face) (inc (:i1 max-face)))]
              (aset a (- j j0-bound) (- i i0-bound) 0)
              (swap! counter dec)))
          (doseq [j (range nj)]
            (doseq [i (range ni)]
              (aset w j i 0)
              (aset h j i 0)))
          (recur @counter (conj! result max-face))
          (persistent! result))))))



(defn triangulate-faces-single-plane [faces-for-plane])


(defn outline-faces-single-plane [faces-for-plane])


(defn generate-voxel-mesh
  "Generate mesh that surrounds our voxel model"
  [voxels]
  (let
    [lookup-table (fill-lookup-table voxels)
     exposed-faces (find-exposed-faces voxels lookup-table)
     plane-faces (into [] (.entrySet exposed-faces))
     faces (into [] (pmap combine-faces-single-plane plane-faces))
     triangles (into [] (pmap triangulate-faces-single-plane faces))
     lines (into [] (pmap outline-faces-single-plane faces))]
    (Mesh. triangles lines)))