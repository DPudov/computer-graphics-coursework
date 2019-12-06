(ns computer-graphics-coursework-backend.render.voxel)

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



