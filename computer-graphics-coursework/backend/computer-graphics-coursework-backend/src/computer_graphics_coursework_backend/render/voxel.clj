(ns computer_graphics_coursework_backend.render.voxel
  (:require [computer_graphics_coursework_backend.render.mesh :as mesh]
            [computer_graphics_coursework_backend.render.triangle :as triangle]
            [computer_graphics_coursework_backend.render.vertex :as vertex])
  (:import (computer_graphics_coursework_backend.math.vector Vector3D Vector4D)
           (computer_graphics_coursework_backend.render.mesh Mesh)
           (java.util HashMap)
           (java.util.concurrent ConcurrentHashMap ConcurrentHashMap$MapEntry)))

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
  (let [m (ConcurrentHashMap.)]
    (doall (pmap (fn [voxel]
                   (let [x (:x voxel)
                         y (:y voxel)
                         z (:z voxel)
                         c (:color voxel)]

                     (if (not (in? lookup-table [(inc x) y z]))
                       (let [plane (VoxelPlane. voxel-pos-x x c)
                             face (VoxelFace. y z y z)]
                         (.put m plane (conj (.get m plane) face))))
                     (if (not (in? lookup-table [(dec x) y z]))
                       (let [plane (VoxelPlane. voxel-neg-x x c)
                             face (VoxelFace. y z y z)]
                         (.put m plane (conj (.get m plane) face))))
                     (if (not (in? lookup-table [x (inc y) z]))
                       (let [plane (VoxelPlane. voxel-pos-y y c)
                             face (VoxelFace. x z x z)]
                         (.put m plane (conj (.get m plane) face))))
                     (if (not (in? lookup-table [x (dec y) z]))
                       (let [plane (VoxelPlane. voxel-neg-y y c)
                             face (VoxelFace. x z x z)]
                         (.put m plane (conj (.get m plane) face))))
                     (if (not (in? lookup-table [x y (inc z)]))
                       (let [plane (VoxelPlane. voxel-pos-z z c)
                             face (VoxelFace. x y x y)]
                         (.put m plane (conj (.get m plane) face))))
                     (if (not (in? lookup-table [x y (dec z)]))
                       (let [plane (VoxelPlane. voxel-neg-z z c)
                             face (VoxelFace. x y x y)]
                         (.put m plane (conj (.get m plane) face)))))) voxels))
    m))

(defn calculate-max-face [i0 j0 nj ni a h w]
  (let [max-face (atom (VoxelFace. 0 0 0 0))
        max-area (atom 0)
        minw (atom 0)]
    (doseq [j (range nj)]
      (doseq [i (range ni)
              :when (not (zero? (aget a j i)))]
        (if (zero? j)
          (aset h j i (int 1))
          (aset h j i (int (inc (aget h (dec j) i)))))
        (if (zero? i)
          (aset w j i (int 1))
          (aset w j i (int (inc (aget w j (dec i))))))
        (reset! minw (aget w j i))
        (doseq [dh (range (aget h j i))]
          (if (< (aget w (- j dh) i) @minw)
            (reset! minw (aget w (- j dh) i)))
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
  (let [plane (.getKey plane-faces)
        faces (.getValue plane-faces)
        i0-bound (:i0 (apply min-key :i0 faces))
        j0-bound (:j0 (apply min-key :j0 faces))
        i1-bound (:i1 (apply max-key :i1 faces))
        j1-bound (:j1 (apply max-key :j1 faces))
        nj (inc (- j1-bound j0-bound))
        ni (inc (- i1-bound i0-bound))
        a (make-array Integer/TYPE nj ni)
        w (make-array Integer/TYPE nj ni)
        h (make-array Integer/TYPE nj ni)
        counter (atom 0)]
    (doseq [f faces]
      (doseq [j (range (:j0 f) (inc (:j1 f)))]
        (doseq [i (range (:i0 f) (inc (:i1 f)))]
          (aset a (- j j0-bound) (- i i0-bound) (int 1))
          (swap! counter inc))))
    [plane (loop [c @counter
                  result (transient [])]
             (if (> c 0)
               (let [max-face (calculate-max-face i0-bound j0-bound nj ni a h w)]
                 (doseq [j (range (:j0 max-face) (inc (:j1 max-face)))]
                   (doseq [i (range (:i0 max-face) (inc (:i1 max-face)))]
                     (aset a (- j j0-bound) (- i i0-bound) (int 0))
                     (swap! counter dec)))
                 (doseq [j (range nj)]
                   (doseq [i (range ni)]
                     (aset w j i (int 0))
                     (aset h j i (int 0))))
                 (recur @counter (conj! result max-face)))
               (persistent! result)))]))



(defn triangle-for-points
  "Generate triangle from points"
  [p1 p2 p3 c]
  (triangle/->Triangle
    (vertex/->Vertex p1 nil nil c nil)
    (vertex/->Vertex p2 nil nil c nil)
    (vertex/->Vertex p3 nil nil c nil)))


(defn triangulate-faces-single-plane
  "Get triangles vector from plane faces"
  [faces-for-plane]
  (let [plane (first faces-for-plane)
        faces (first (rest faces-for-plane))
        k (+ (double (:position plane)) (* 0.5 (double (:sign (:normal plane)))))
        c (:color plane)
        axis (:axis (:normal plane))]
    (->> faces
         (pmap (fn [face]
                 (let [i0 (- (:i0 face) 0.5)
                       j0 (- (:j0 face) 0.5)
                       i1 (+ (:i1 face) 0.5)
                       j1 (+ (:j1 face) 0.5)
                       p1 (cond
                            (= axis voxel-x)
                            (Vector4D. k i0 j0 1)
                            (= axis voxel-y)
                            (Vector4D. i0 k j1 1)
                            (= axis voxel-z)
                            (Vector4D. i0 j0 k 1))
                       p2 (cond
                            (= axis voxel-x)
                            (Vector4D. k i1 j0 1)
                            (= axis voxel-y)
                            (Vector4D. i1 k j1 1)
                            (= axis voxel-z)
                            (Vector4D. i1 j0 k 1))
                       p3 (cond
                            (= axis voxel-x)
                            (Vector4D. k i1 j1 1)
                            (= axis voxel-y)
                            (Vector4D. i1 k j0 1)
                            (= axis voxel-z)
                            (Vector4D. i1 j1 k 1))
                       p4 (cond
                            (= axis voxel-x)
                            (Vector4D. k i0 j1 1)
                            (= axis voxel-y)
                            (Vector4D. i0 k j0 1)
                            (= axis voxel-z)
                            (Vector4D. i0 j1 k 1))]
                   (if (neg? (:sign (:normal plane)))
                     (let [t1 (triangle-for-points p4 p3 p2 c)
                           t2 (triangle-for-points p4 p2 p1 c)]
                       [t1 t2])
                     (let [t1 (triangle-for-points p1 p2 p3 c)
                           t2 (triangle-for-points p1 p3 p4 c)]
                       [t1 t2])))))
         (flatten)
         (into []))))

;
;(defn outline-faces-single-plane
;  "Create outside wireframe"
;  [faces-for-plane])


(defn generate-voxel-mesh
  "Generate mesh that surrounds our voxel model"
  [voxels]
  (let
    [lookup-table (fill-lookup-table voxels)
     exposed-faces (find-exposed-faces voxels lookup-table)
     plane-faces (into [] (.entrySet exposed-faces))
     faces (into [] (pmap combine-faces-single-plane plane-faces))
     triangles (into [] (flatten (pmap triangulate-faces-single-plane faces)))
     lines nil]
    (println "exposed-faces" (.size exposed-faces) "plane-faces" (.size (.entrySet exposed-faces)))
    ;lines (into [] (pmap outline-faces-single-plane faces))]
    (Mesh. triangles lines)))