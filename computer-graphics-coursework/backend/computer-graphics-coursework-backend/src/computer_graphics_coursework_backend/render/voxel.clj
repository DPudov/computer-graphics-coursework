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
       (map (fn [voxel] [(:x voxel) (:y voxel) (:z voxel)]))
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

(defn generate-voxel-mesh
  "Generate mesh that surrounds our voxel model"
  [voxels]
  (let
    [lookup-table (fill-lookup-table voxels)
     exposed-faces (find-exposed-faces voxels lookup-table)
     triangles (triangulate-exposed-faces exposed-faces)
     lines (outline-exposed-faces exposed-faces)]
    (Mesh. triangles lines)))