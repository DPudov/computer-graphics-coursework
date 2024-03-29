(ns computer_graphics_coursework_backend.render.drawer
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [computer_graphics_coursework_backend.render.voxel :as voxel]
            [computer_graphics_coursework_backend.render.camera :as camera]
            [computer_graphics_coursework_backend.render.vertex :as vertex]
            [computer-graphics-coursework-backend.render.shader :as shader]
            [computer-graphics-coursework-backend.render.lights :as lights]
            [computer_graphics_coursework_backend.render.color :as color]
            [hiphip.array :as hiphip]
            [clojure.core.matrix :as m]
            [parallel.core :as p]
            [primitive-math :as pmath])

  (:import java.awt.image.BufferedImage
           (java.awt Color Graphics)
           (computer_graphics_coursework_backend.render.mesh Mesh)
           (computer_graphics_coursework_backend.render.camera Camera)
           (computer_graphics_coursework_backend.render.vertex Vertex)
           (computer_graphics_coursework_backend.math.vector Vector)
           (mikera.vectorz Vector3)))
(defrecord ClipPlane [p n])

(defrecord Triangle [v1 v2 v3])

(def clip-planes
  [(ClipPlane. (m/matrix :vectorz [1 0 0 1]) (m/matrix :vectorz [-1 0 0 1]))
   (ClipPlane. (m/matrix :vectorz [-1 0 0 1]) (m/matrix :vectorz [1 0 0 1]))
   (ClipPlane. (m/matrix :vectorz [0 1 0 1]) (m/matrix :vectorz [0 -1 0 1]))
   (ClipPlane. (m/matrix :vectorz [0 -1 0 1]) (m/matrix :vectorz [0 1 0 1]))
   (ClipPlane. (m/matrix :vectorz [0 0 1 1]) (m/matrix :vectorz [0 0 -1 1]))
   (ClipPlane. (m/matrix :vectorz [0 0 -1 1]) (m/matrix :vectorz [0 0 1 1]))])

(defn put-pixel [^BufferedImage image-buffer x y ^Color color]
  (.setRGB image-buffer x y (.getRGB color)))

(defn draw-line-fast
  [^Graphics g xb yb xe ye ^Color color]
  (.setColor g color)
  (.drawLine g xb yb xe ye))

(def model-matrix
  (m/matrix :vectorz [[1.0 0.0 0.0 0.0]
                      [0.0 1.0 0.0 0.0]
                      [0.0 0.0 1.0 0.0]
                      [0.0 0.0 0.0 1.0]]))

(defn point-in-front
  [plane point]
  (pos? (m/dot (m/sub point (:p plane)) (:n plane))))

(defn intersect-segment
  [plane v0 v1]
  (let [u (m/sub v1 v0)
        w (m/sub v0 (:p plane))
        d (m/dot (:n plane) u)
        n (m/dot (m/sub (:n plane)) w)]
    (m/add v0 (m/scale u (/ n d)))))

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
  (let [v0 (m/sub p2 p1)
        v1 (m/sub p3 p1)
        v2 (m/sub p p1)
        d00 (m/dot v0 v0)
        d01 (m/dot v0 v1)
        d11 (m/dot v1 v1)
        d20 (m/dot v2 v0)
        d21 (m/dot v2 v1)
        d (- (* d00 d11) (* d01 d01))
        v (/ (- (* d11 d20) (* d01 d21)) d)
        w (/ (- (* d00 d21) (* d01 d20)) d)
        u (- 1.0 v w)]
    (m/matrix :vectorz [u v w 1])))

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


(defn colorize [^Vertex v1]
  (let [v1-xyz (:position v1)
        c (:color v1)
        r (.getRed c)
        g (.getGreen c)
        b (.getBlue c)
        v1-normal (m/normalise (voxel/voxel-normal (:normal v1)))
        light-direction-v1 (m/normalise (m/sub @lights/light v1-xyz))
        view-direction-v1 (m/normalise (m/sub v1-xyz))
        reflect-v1 (vec/reflect (m/sub light-direction-v1) v1-normal)
        diffuse-v1 (m/scale lights/diffuse-albedo (max (m/dot v1-normal light-direction-v1) 0.0))
        specular-v1 (m/scale lights/specular-albedo
                             (Math/pow (max (m/dot reflect-v1 view-direction-v1) 2.0) lights/specular-power))
        modification (m/add lights/ambient diffuse-v1 specular-v1)]

    (+ (pmath/bit-shift-left (int (* 255 (m/mget modification 3))) 24)
       (pmath/bit-shift-left (int (* r (m/mget modification 0))) 16)
       (pmath/bit-shift-left (int (* g (m/mget modification 1))) 8)
       (int (* b (m/mget modification 2))))))


(defn draw-triangles
  [canvas triangles mvp]
  (let
    [viewport [(.getWidth canvas) (.getHeight canvas)]
     width ^long (viewport 0)
     height ^long (viewport 1)
     neg-inf Double/NEGATIVE_INFINITY
     len ^long (* width height)
     z-buffer ^doubles (hiphip/amake Double/TYPE [_ len] neg-inf)]

    (doall (pmap (fn [^Triangle triangle]
                   (let [v1 ^Vertex (:v1 triangle)
                         v2 ^Vertex (:v2 triangle)
                         v3 ^Vertex (:v3 triangle)
                         p1 (camera/project-to-screen (:position v1) mvp viewport)
                         p2 (camera/project-to-screen (:position v2) mvp viewport)
                         p3 (camera/project-to-screen (:position v3) mvp viewport)
                         p1-x ^double (m/mget p1 0)
                         p1-y ^double (m/mget p1 1)
                         p1-z ^double (m/mget p1 2)
                         p2-x ^double (m/mget p2 0)
                         p2-y ^double (m/mget p2 1)
                         p2-z ^double (m/mget p2 2)
                         p3-x ^double (m/mget p3 0)
                         p3-y ^double (m/mget p3 1)
                         p3-z ^double (m/mget p3 2)
                         c ^int (colorize v2)
                         min-x ^long (long (max 0.0 (Math/ceil (min p1-x p2-x p3-x))))
                         max-x ^long (long (min (dec width) (Math/floor (max p1-x p2-x p3-x))))
                         min-y ^long (long (max 0.0 (Math/ceil (min p1-y p2-y p3-y))))
                         max-y ^long (long (min (dec height) (Math/floor (max p1-y p2-y p3-y))))
                         area ^double (+ (* (- p1-y p3-y) (- p2-x p3-x)) (* (- p2-y p3-y) (- p3-x p1-x)))]

                     (doall (p/pmap (fn [^long y]
                                      (doall
                                        (map (fn [^long x]
                                               (let [b1 ^double (/ (+ (* (- y p3-y) (- p2-x p3-x))
                                                                      (* (- p2-y p3-y) (- p3-x x)))
                                                                   area)
                                                     b2 ^double (/ (+ (* (- y p1-y) (- p3-x p1-x))
                                                                      (* (- p3-y p1-y) (- p1-x x)))
                                                                   area)
                                                     b3 ^double (/ (+ (* (- y p2-y) (- p1-x p2-x))
                                                                      (* (- p1-y p2-y) (- p2-x x)))
                                                                   area)
                                                     depth ^double (+ (* b1 p1-z) (* b2 p2-z) (* b3 p3-z))
                                                     z-index ^long (+ (* y width) x)]
                                                 (if (< (aget z-buffer z-index) (+ depth 10e-5))
                                                   (do
                                                     (.setRGB canvas x y c)
                                                     (aset z-buffer z-index depth)))))
                                             (range min-x (inc max-x)))))
                                    (range min-y (inc max-y))))))
                 triangles))))

(defn draw-mesh [canvas ^Mesh mesh mvp]
  (let [triangles (:triangles mesh)]+ 0.1
    (draw-triangles canvas triangles mvp)))

(defn draw-voxels
  [^BufferedImage canvas voxels ^Camera camera]
  (let [mesh (voxel/generate-voxel-mesh voxels)
        mvp (camera/model-view-projection-matrix (camera/perspective (:position camera))
                                                 (camera/get-view-matrix camera)
                                                 model-matrix)]
    (draw-mesh canvas mesh mvp)))



