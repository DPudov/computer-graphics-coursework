(ns computer-graphics-coursework-backend.render.lights
  (:require [computer_graphics_coursework_backend.math.vector :as vec]
            [clojure.core.matrix :as m])
  (:import (computer_graphics_coursework_backend.math.vector Vector3D Vector4D)))
;
;(def ambient-red (atom 1))
;(def ambient-green (atom 1))
;(def ambient-blue (atom 1))
;(def ambient-intensity (atom 0.2))
;
;(def diffuse-red (atom 1))
;(def diffuse-green (atom 1))
;(def diffuse-blue (atom 1))
;(def diffuse-intensity (atom 0.8))
;(def diffuse-x (atom 0)) ; diffuse length = 1!
;(def diffuse-y (atom 0))
;(def diffuse-z (atom 1))
(def ^:const diffuse-albedo (m/array :vectorz [0.2 0.2 0.2 0.2]))
(def ^:const specular-albedo (m/array :vectorz [0.7 0.7 0.7 0.7]))
(def ^:const specular-power 0.0)
(def ^:const ambient (m/array :vectorz [0.1 0.1 0.1 0.1]))
(def light (atom (m/normalise (m/array :vectorz [0 30 -10 1]))))