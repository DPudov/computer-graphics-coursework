(ns computer_graphics_coursework_backend.render.drawer
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.render.voxel :as voxel]
            [computer_graphics_coursework_backend.render.camera :as camera]
            [computer_graphics_coursework_backend.render.vertex :as vertex])
  (:import java.awt.image.BufferedImage
           (computer_graphics_coursework_backend.math.vector Vector3D Vector2D Vector4D)
           (java.awt Color)
           (computer_graphics_coursework_backend.math.matrix Matrix4D)
           (computer_graphics_coursework_backend.render.triangle Triangle)))
(def width 640)

(def height 480)


(defrecord ClipPlane
  [p n])

(def clip-planes
  [(ClipPlane. (Vector4D. 1 0 0 1) (Vector4D. -1 0 0 1))
   (ClipPlane. (Vector4D. -1 0 0 1) (Vector4D. 1 0 0 1))
   (ClipPlane. (Vector4D. 0 1 0 1) (Vector4D. 0 -1 0 1))
   (ClipPlane. (Vector4D. 0 -1 0 1) (Vector4D. 0 1 0 1))
   (ClipPlane. (Vector4D. 0 0 1 1) (Vector4D. 0 0 -1 1))
   (ClipPlane. (Vector4D. 0 0 -1 1) (Vector4D. 0 0 1 1))])

(defn put-pixel [^BufferedImage image-buffer x y ^Color color]
  (.setRGB image-buffer x y (.getRGB color)))

(defn draw-point
  [^BufferedImage image-buffer point ^Color color]
  (let [x (int (.getX point))
        y (int (.getY point))]
    (if (and (> x 0) (< x width) (> y 0) (< y height))
      (put-pixel image-buffer x y color))))

(defn draw-line-fast
  [^BufferedImage image-buffer xb yb xe ye ^Color color]
  (let [g (.getGraphics image-buffer)]
    (.setColor g color)
    (.drawLine g xb yb xe ye)))

(defn draw-line
  [^BufferedImage image-buffer xb yb xe ye ^Color color]
  (let
    [dist-x (Math/abs (- xb xe))
     dist-y (Math/abs (- yb ye))
     steep (> dist-y dist-x)
     rgb (.getRGB color)]
    (let [[xb yb xe ye] (if steep [yb xb ye xe] [xb yb xe ye])]
      (let [[xb yb xe ye] (if (> xb xe) [xe ye xb yb] [xb yb xe ye])]
        (let [delta-x (- xe xb)
              delta-y (Math/abs (- ye yb))
              y-step (if (< yb ye) 1 -1)]

          (let [plot (if steep
                       #(.setRGB image-buffer (int %1) (int %2) rgb)
                       #(.setRGB image-buffer (int %2) (int %1) rgb))]
            (loop [xi xb
                   yi yb
                   error (Math/floor (/ delta-x 2))]
              (plot xi yi)
              (if (< xi xe)
                (if (< error delta-y)
                  (recur (inc xi) (+ yi y-step) (+ error (- delta-x delta-y)))
                  (recur (inc xi) yi (- error delta-y)))))))))))

(defn draw-voxel
  [^BufferedImage canvas vertices color]
  (let [vafp (vertices 0)
        vbfp (vertices 1)
        vcfp (vertices 2)
        vdfp (vertices 3)
        vabp (vertices 4)
        vbbp (vertices 5)
        vcbp (vertices 6)
        vdbp (vertices 7)]
    (draw-line-fast canvas (vafp 0) (vafp 1) (vbfp 0) (vbfp 1) color)
    (draw-line-fast canvas (vbfp 0) (vbfp 1) (vcfp 0) (vcfp 1) color)
    (draw-line-fast canvas (vcfp 0) (vcfp 1) (vdfp 0) (vdfp 1) color)
    (draw-line-fast canvas (vdfp 0) (vdfp 1) (vafp 0) (vafp 1) color)
    (draw-line-fast canvas (vabp 0) (vabp 1) (vbbp 0) (vbbp 1) color)
    (draw-line-fast canvas (vbbp 0) (vbbp 1) (vcbp 0) (vcbp 1) color)
    (draw-line-fast canvas (vcbp 0) (vcbp 1) (vdbp 0) (vdbp 1) color)
    (draw-line-fast canvas (vdbp 0) (vdbp 1) (vabp 0) (vabp 1) color)
    (draw-line-fast canvas (vafp 0) (vafp 1) (vabp 0) (vabp 1) color)
    (draw-line-fast canvas (vbfp 0) (vbfp 1) (vbbp 0) (vbbp 1) color)
    (draw-line-fast canvas (vcfp 0) (vcfp 1) (vcbp 0) (vcbp 1) color)
    (draw-line-fast canvas (vdfp 0) (vdfp 1) (vdbp 0) (vdbp 1) color)))


(def model-matrix
  (Matrix4D. 1.0 0.0 0.0 0.0
             0.0 1.0 0.0 0.0
             0.0 0.0 1.0 0.0
             0.0 0.0 0.0 1.0))



(defn point-in-front
  [plane point]
  (pos? (vec/dot (vec/sub point (:p plane)) (:n plane))))

(defn intersect-segment
  [plane v0 v1]
  (let [u (vec/sub v1 v0)
        w (vec/sub v0 (:p plane))
        d (vec/dot (:n plane) u)
        n (vec/dot (vec/sub (:n plane)) w)]
    (vec/add v0 (vec/scale u (/ n d)))))

(defn sutherland-hodgman
  "Sutherland-Hodgman clipping algorithm"
  [points clip-planes]
  (let [output (atom points)
        input (atom nil)]
    (doseq [plane clip-planes]
      (reset! input @output)
      (reset! output nil)
      (if (not (zero? (count @input)))
        (let [start (atom (last @input))]
          (doseq [el @input]
            (cond
              (point-in-front plane el)
              (do
                (if (not (point-in-front plane @start))
                  (let [x (intersect-segment plane @start el)]
                    (swap! output conj x)))
                (swap! output conj el))
              (point-in-front plane @start)
              (let [x (intersect-segment plane @start el)]
                (swap! output conj x)))
            (reset! start el))))
      @output)))

(defn barycentric
  [p1 p2 p3 p]
  (let [v0 (vec/sub-3d p2 p1)
        v1 (vec/sub-3d p3 p1)
        v2 (vec/sub-3d p p1)
        d00 (vec/dot-3d v0 v0)
        d01 (vec/dot-3d v0 v1)
        d11 (vec/dot-3d v1 v1)
        d20 (vec/dot-3d v2 v0)
        d21 (vec/dot-3d v2 v1)
        d (- (* d00 d11) (* d01 d01))
        v (/ (- (* d11 d20) (* d01 d21)) d)
        w (/ (- (* d00 d21) (* d01 d20)) d)
        u (- 1 v w)]
    (Vector4D. u v w 1)))

(defn clip-triangle [v1 v2 v3]
  (let [w1 (:output v1)
        w2 (:output v2)
        w3 (:output v3)
        points [w1 w2 w3]
        new-points (sutherland-hodgman points clip-planes)
        result (atom [])]
    (doseq [i (range 2 (count new-points))]
      (let [b1 (barycentric w1 w2 w3 (first new-points))
            b2 (barycentric w1 w2 w3 (new-points (dec i)))
            b3 (barycentric w1 w2 w3 (new-points i))
            v1 (vertex/interpolate-vertices v1 v2 v3 b1)
            v2 (vertex/interpolate-vertices v1 v2 v3 b2)
            v3 (vertex/interpolate-vertices v1 v2 v3 b3)]
        (swap! result conj (Triangle. v1 v2 v3))))))

(defn draw-clipped-triangle
  [canvas triangle]
  (let [v1 (:v1 triangle)
        v2 (:v2 triangle)
        v3 (:v3 triangle)
        x1 ((:position))
        back-face-culling ((:x v1))]))

(defn draw-triangles
  [triangles canvas]
  (pmap (fn draw-triangle [triangle]
          (let [v1 (:v1 triangle)
                v2 (:v2 triangle)
                v3 (:v3 triangle)]
            (if (or (vertex/is-outside v1) (vertex/is-outside v2) (vertex/is-outside v3))
              (let [triangles (clip-triangle v1 v2 v3)]
                (pmap (fn draw-clipped-triangle
                        [triangle]
                        (let [v1 (:v1 triangle)
                              v2 (:v2 triangle)
                              v3 (:v3 triangle)
                              x1 ((:position))
                              back-face-culling ((:x v1))])) triangles))
              (draw-clipped-triangle canvas triangle)))) triangles))

(defn draw-mesh [canvas mesh]
  (let [triangles (:triangles mesh)]
    (draw-triangles canvas triangles)))

(defn draw-voxels
  [^BufferedImage canvas voxels camera]
  (let [mesh (voxel/generate-voxel-mesh voxels)
        mvp (camera/model-view-projection-matrix (camera/perspective (:position camera))
                                                 (camera/get-view-matrix camera)
                                                 model-matrix)]
    (draw-mesh canvas mesh)))



