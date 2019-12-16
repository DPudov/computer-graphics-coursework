(ns computer_graphics_coursework_backend.render.drawer
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.render.voxel :as voxel]
            [computer_graphics_coursework_backend.render.camera :as camera]
            [computer_graphics_coursework_backend.render.vertex :as vertex]
            [computer-graphics-coursework-backend.render.shader :as shader]
            [computer-graphics-coursework-backend.render.lights :as lights]
            [computer_graphics_coursework_backend.render.color :as color]
            [hiphip.array :as hiphip])
  (:import java.awt.image.BufferedImage
           (computer_graphics_coursework_backend.math.vector Vector4D)
           (java.awt Color Graphics)
           (computer_graphics_coursework_backend.math.matrix Matrix4D)))

(defrecord ClipPlane [p n])

(defrecord Triangle [v1 v2 v3])

(def clip-planes
  [(ClipPlane. (Vector4D. 1 0 0 1) (Vector4D. -1 0 0 1))
   (ClipPlane. (Vector4D. -1 0 0 1) (Vector4D. 1 0 0 1))
   (ClipPlane. (Vector4D. 0 1 0 1) (Vector4D. 0 -1 0 1))
   (ClipPlane. (Vector4D. 0 -1 0 1) (Vector4D. 0 1 0 1))
   (ClipPlane. (Vector4D. 0 0 1 1) (Vector4D. 0 0 -1 1))
   (ClipPlane. (Vector4D. 0 0 -1 1) (Vector4D. 0 0 1 1))])

(defn put-pixel [^BufferedImage image-buffer x y ^Color color]
  (.setRGB image-buffer x y (.getRGB color)))

(defn draw-line-fast
  [^Graphics g xb yb xe ye ^Color color]
  (.setColor g color)
  (.drawLine g xb yb xe ye))

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
  (let [w1 (:position v1)
        w2 (:position v2)
        w3 (:position v3)
        points [w1 w2 w3]
        new-points (sutherland-hodgman points clip-planes)]
    (loop [i 2
           result (transient [])]
      (if (< i (count new-points))
        (let [b1 (barycentric w1 w2 w3 (first new-points))
              b2 (barycentric w1 w2 w3 (new-points (dec i)))
              b3 (barycentric w1 w2 w3 (new-points i))
              v1 (vertex/interpolate-vertices v1 v2 v3 b1)
              v2 (vertex/interpolate-vertices v1 v2 v3 b2)
              v3 (vertex/interpolate-vertices v1 v2 v3 b3)
              new-triangle (Triangle. v1 v2 v3)]
          (recur (inc i) (conj! result new-triangle)))
        (persistent! result)))))

(defn draw-clipped-triangle
  [canvas triangle mvp viewport]
  (let [p1 (camera/project-to-screen (:position (:v1 triangle)) mvp viewport)
        p2 (camera/project-to-screen (:position (:v2 triangle)) mvp viewport)
        p3 (camera/project-to-screen (:position (:v3 triangle)) mvp viewport)
        c (:color (:v1 triangle))
        g (.getGraphics canvas)]
    (draw-line-fast g (p1 0) (p1 1) (p2 0)
                    (p2 1) c)
    (draw-line-fast g (p2 0) (p2 1) (p3 0)
                    (p3 1) c)
    (draw-line-fast g (p1 0) (p1 1) (p3 0)
                    (p3 1) c)))


(defn draw-triangles
  [canvas triangles mvp shader]
  (let
    [viewport [(.getWidth canvas) (.getHeight canvas)]
     width (viewport 0)
     height (viewport 1)
     neg-inf Double/NEGATIVE_INFINITY
     len (* width height)
     z-buffer (hiphip/amake Double/TYPE [_ len] neg-inf)]
    (doall (pmap (fn [triangle]
                   (let [p1 (camera/project-to-screen (:position (:v1 triangle)) mvp viewport)
                         p2 (camera/project-to-screen (:position (:v2 triangle)) mvp viewport)
                         p3 (camera/project-to-screen (:position (:v3 triangle)) mvp viewport)
                         p1-x (p1 0)
                         p1-y (p1 1)
                         p1-z (p1 2)
                         p2-x (p2 0)
                         p2-y (p2 1)
                         p2-z (p2 2)
                         p3-x (p3 0)
                         p3-y (p3 1)
                         p3-z (p3 2)
                         c (.getRGB (:color (:v1 triangle)))
                         min-x (int (max 0 (Math/ceil (min p1-x p2-x p3-x))))
                         max-x (int (min (dec width) (Math/floor (max p1-x p2-x p3-x))))
                         min-y (int (max 0 (Math/ceil (min p1-y p2-y p3-y))))
                         max-y (int (min (dec height) (Math/floor (max p1-y p2-y p3-y))))
                         area (+ (* (- p1-y p3-y) (- p2-x p3-x)) (* (- p2-y p3-y) (- p3-x p1-x)))]
                     (doall (pmap (fn [y]
                                    (doall
                                      (pmap (fn [x]
                                              (let [b1 (/ (+ (* (- y p3-y) (- p2-x p3-x))
                                                             (* (- p2-y p3-y) (- p3-x x)))
                                                          area)
                                                    b2 (/ (+ (* (- y p1-y) (- p3-x p1-x))
                                                             (* (- p3-y p1-y) (- p1-x x)))
                                                          area)
                                                    b3 (/ (+ (* (- y p2-y) (- p1-x p2-x))
                                                             (* (- p1-y p2-y) (- p2-x x)))
                                                          area)
                                                    depth (+ (* b1 p1-z) (* b2 p2-z) (* b3 p3-z))
                                                    z-index (int (+ (* y width) x))]
                                                (if (< (aget z-buffer z-index) depth)
                                                  (do
                                                    (.setRGB canvas x y c)
                                                    (aset z-buffer z-index depth)))))
                                            (range min-x (inc max-x)))))
                                  (range min-y (inc max-y))))))
                 triangles))))

(defn draw-mesh [canvas mesh mvp camera-position]
  (let [triangles (:triangles mesh)
        shader (shader/phong-shader mvp @lights/light camera-position)]

    (draw-triangles canvas triangles mvp shader)))

(defn draw-voxels
  [^BufferedImage canvas voxels camera]
  (let [mesh (voxel/generate-voxel-mesh voxels)
        mvp (camera/model-view-projection-matrix (camera/perspective (:position camera))
                                                 (camera/get-view-matrix camera)
                                                 model-matrix)]
    (draw-mesh canvas mesh mvp (:position camera))))



