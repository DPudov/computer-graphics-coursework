(ns computer_graphics_coursework_backend.render.voxel
  (:require [computer_graphics_coursework_backend.render.mesh :as mesh]
            [computer_graphics_coursework_backend.render.triangle :as triangle]
            [computer_graphics_coursework_backend.render.vertex :as vertex]
            [clojure.core.matrix :as m]
            [parallel.core :as p])
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

(def ^:const voxel-x 1)
(def ^:const voxel-y 2)
(def ^:const voxel-z 3)

(def ^:const voxel-pos-x (VoxelNormal. voxel-x 1))
(def ^:const voxel-neg-x (VoxelNormal. voxel-x -1))
(def ^:const voxel-pos-y (VoxelNormal. voxel-y 1))
(def ^:const voxel-neg-y (VoxelNormal. voxel-y -1))
(def ^:const voxel-pos-z (VoxelNormal. voxel-z 1))
(def ^:const voxel-neg-z (VoxelNormal. voxel-z -1))

(defn voxel-normal
  [voxel-normal]
  (let [axis (:axis voxel-normal)
        sign (:sign voxel-normal)]
    (cond
      (= axis voxel-x) (m/array :vectorz [sign 0 0 1])
      (= axis voxel-y) (m/array :vectorz [0 sign 0 1])
      (= axis voxel-z) (m/array :vectorz [0 0 sign 1]))))

(def voxel-base-count (atom 256))
(def ^:const dim 50)
;(defn fill-lookup-table
;  [voxels]
;  (->> voxels
;       (pmap (fn [voxel] [(:x voxel) (:y voxel) (:z voxel)]))
;       (into [])))

(defn fill-lookup-table
  [voxels]
  (let
    [lup (make-array Integer/TYPE dim dim dim)]
    (->> voxels
         (p/pmap (fn [voxel]
                   (let [x (:x voxel)
                         y (:y voxel)
                         z (:z voxel)
                         lup-axis (if (and (>= x 0) (< x dim)) (aget ^objects lup x) nil)
                         ^ints lup-axis-ordinates (if (and (>= x 0) (< x dim) (>= y 0) (< y dim)) (aget ^objects lup-axis y) nil)]
                     (if (and (>= x 0) (< x dim) (>= y 0) (< y dim) (>= z 0) (< z dim)) (aset lup-axis-ordinates z (int 1))))))

         (doall))
    lup))

;(defn in?
;  "true if coll contains elm"
;  [coll elm]
;  (some #(= elm %) coll))

(defn in?
  [coll elm]
  (let [x (elm 0)
        y (elm 1)
        z (elm 2)
        lup-axis (if (and (>= x 0) (< x dim)) (aget ^objects coll x) nil)
        ^ints lup-axis-ordinates (if (and (>= x 0) (< x dim) (>= y 0) (< y dim)) (aget ^objects lup-axis y) nil)]
    (if (and (>= x 0) (< x dim) (>= y 0) (< y dim) (>= z 0) (< z dim))
      (not (zero? (aget lup-axis-ordinates z)))
      false)))

(defn find-exposed-faces
  [voxels lookup-table]
  (let [m (ConcurrentHashMap.)]
    (doall (map (fn [^Voxel voxel]
                  (let [x (:x voxel)
                        y (:y voxel)
                        z (:z voxel)
                        c (:color voxel)]

                    (if-not (in? lookup-table [(inc x) y z])
                      (let [plane (VoxelPlane. voxel-pos-x x c)
                            face (VoxelFace. y z y z)]
                        (.put m plane (conj (.get m plane) face))))
                    (if-not (in? lookup-table [(dec x) y z])
                      (let [plane (VoxelPlane. voxel-neg-x x c)
                            face (VoxelFace. y z y z)]
                        (.put m plane (conj (.get m plane) face))))
                    (if-not (in? lookup-table [x (inc y) z])
                      (let [plane (VoxelPlane. voxel-pos-y y c)
                            face (VoxelFace. x z x z)]
                        (.put m plane (conj (.get m plane) face))))
                    (if-not (in? lookup-table [x (dec y) z])
                      (let [plane (VoxelPlane. voxel-neg-y y c)
                            face (VoxelFace. x z x z)]
                        (.put m plane (conj (.get m plane) face))))
                    (if-not (in? lookup-table [x y (inc z)])
                      (let [plane (VoxelPlane. voxel-pos-z z c)
                            face (VoxelFace. x y x y)]
                        (.put m plane (conj (.get m plane) face))))
                    (if-not (in? lookup-table [x y (dec z)])
                      (let [plane (VoxelPlane. voxel-neg-z z c)
                            face (VoxelFace. x y x y)]
                        (.put m plane (conj (.get m plane) face)))))) voxels))
    m))

(defn calculate-max-face [i0 j0 nj ni a h w]
  (let [max-face (atom (VoxelFace. 0 0 0 0))
        max-area (atom 0)
        minw (atom 0)]
    (doseq [j (range nj)
            :let [^ints a-row (aget ^objects a j)
                  ^ints h-row (aget ^objects h j)
                  ^ints w-row (aget ^objects w j)]]
      (doseq [i (range ni)
              :when (not (zero? (aget a-row i)))]
        (if (zero? j)
          (aset h-row i (int 1))
          (aset h-row i (int (inc (aget h (dec j) i)))))
        (if (zero? i)
          (aset w-row i (int 1))
          (aset w-row i (int (inc (aget w j (dec i))))))
        (reset! minw (aget w-row i))
        (doseq [dh (range (aget h-row i))
                :let [^ints w-row-cur (aget ^objects w (- j dh))]]
          (if (< (aget w-row-cur i) @minw)
            (reset! minw (aget w-row-cur i)))
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
      (doseq [j (range (:j0 f) (inc (:j1 f)))
              :let [^ints a-row (aget ^objects a (- j j0-bound))]]
        (doseq [i (range (:i0 f) (inc (:i1 f)))]
          (aset a-row (- i i0-bound) (int 1))
          (swap! counter inc))))
    [plane (loop [c @counter
                  result (transient [])]
             (if (> c 0)
               (let [max-face (calculate-max-face i0-bound j0-bound nj ni a h w)]
                 (doseq [j (range (:j0 max-face) (inc (:j1 max-face)))
                         :let [^ints a-row (aget ^objects a (- j j0-bound))]]
                   (doseq [i (range (:i0 max-face) (inc (:i1 max-face)))]
                     (aset a-row (- i i0-bound) (int 0))
                     (swap! counter dec)))
                 (doseq [j (range nj)
                         :let [^ints w-row (aget ^objects w j)
                               ^ints h-row (aget ^objects h j)]]
                   (doseq [i (range ni)]
                     (aset-int w-row i (int 0))
                     (aset-int h-row i (int 0))))
                 (recur @counter (conj! result max-face)))
               (persistent! result)))]))



(defn triangle-for-points
  "Generate triangle from points"
  [p1 p2 p3 c n]
  (triangle/->Triangle
    (vertex/->Vertex p1 n nil c nil)
    (vertex/->Vertex p2 n nil c nil)
    (vertex/->Vertex p3 n nil c nil)))


(defn triangulate-faces-single-plane
  "Get triangles vector from plane faces"
  [faces-for-plane]
  (let [^VoxelPlane plane (first faces-for-plane)
        faces (first (rest faces-for-plane))
        k (+ (double (:position plane)) (* 0.5 (double (:sign (:normal plane)))))
        c (:color plane)
        axis (:axis (:normal plane))]
    (->> faces
         (map (fn [^VoxelFace face]
                (let [i0 (- (:i0 face) 0.5)
                      j0 (- (:j0 face) 0.5)
                      i1 (+ (:i1 face) 0.5)
                      j1 (+ (:j1 face) 0.5)
                      p1 (cond
                           (= axis voxel-x)
                           (m/array :vectorz [k i0 j0 1])
                           (= axis voxel-y)
                           (m/array :vectorz [i0 k j1 1])
                           (= axis voxel-z)
                           (m/array :vectorz [i0 j0 k 1]))
                      p2 (cond
                           (= axis voxel-x)
                           (m/array :vectorz [k i1 j0 1])
                           (= axis voxel-y)
                           (m/array :vectorz [i1 k j1 1])
                           (= axis voxel-z)
                           (m/array :vectorz [i1 j0 k 1]))
                      p3 (cond
                           (= axis voxel-x)
                           (m/array :vectorz [k i1 j1 1])
                           (= axis voxel-y)
                           (m/array :vectorz [i1 k j0 1])
                           (= axis voxel-z)
                           (m/array :vectorz [i1 j1 k 1]))
                      p4 (cond
                           (= axis voxel-x)
                           (m/array :vectorz [k i0 j1 1])
                           (= axis voxel-y)
                           (m/array :vectorz [i0 k j0 1])
                           (= axis voxel-z)
                           (m/array :vectorz [i0 j1 k 1]))]
                  (if (neg? (:sign (:normal plane)))
                    (let [t1 (triangle-for-points p4 p3 p2 c (:normal plane))
                          t2 (triangle-for-points p4 p2 p1 c (:normal plane))]
                      [t1 t2])
                    (let [t1 (triangle-for-points p1 p2 p3 c (:normal plane))
                          t2 (triangle-for-points p1 p3 p4 c (:normal plane))]
                      [t1 t2])))))
         (flatten)
         (into []))))

(defn generate-voxel-mesh
  "Generate mesh that surrounds our voxel model"
  [voxels]
  (let
    [lookup-table (fill-lookup-table voxels)
     exposed-faces (find-exposed-faces voxels lookup-table)
     plane-faces (.entrySet exposed-faces)
     faces (pmap combine-faces-single-plane plane-faces)
     triangles (flatten (pmap triangulate-faces-single-plane faces))
     lines nil]

    (Mesh. triangles lines)))