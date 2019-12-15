(ns computer-graphics-coursework-backend.render.shader
  (:require [computer_graphics_coursework_backend.math.vector :as vec])
  (:import (java.awt Color)
           (computer_graphics_coursework_backend.math.vector Vector4D Vector3D)))

(defn gray
  [x]
  (Color. x x x x))

(def ^:const ambient-color (gray (float 0.4)))
(def ^:const diffuse-color (gray (float 0.9)))
(def ^:const specular-power 0)

(defrecord PhongShader
  [matrix light camera ambient diffuse specular specular-power])

(defn phong-shader
  [matrix light camera-position]
  (let [ambient (Color. (float 0.2) (float 0.2) (float 0.2) (float 1.0))
        diffuse (Color. (float 0.8) (float 0.8) (float 0.8) (float 1.0))
        specular (Color. (float 1.0) (float 1.0) (float 1.0) (float 1.0))]
    (PhongShader. matrix light camera-position ambient diffuse specular 32)))

