(ns computer-graphics-coursework-backend.render.lights
  (:require [computer_graphics_coursework_backend.math.vector :as vec])
  (:import (computer_graphics_coursework_backend.math.vector Vector3D)))

(def ambient-red (atom 1))
(def ambient-green (atom 1))
(def ambient-blue (atom 1))
(def ambient-intensity (atom 0.2))

(def diffuse-red (atom 1))
(def diffuse-green (atom 1))
(def diffuse-blue (atom 1))
(def diffuse-intensity (atom 0.8))
(def diffuse-x (atom 0)) ; diffuse length = 1!
(def diffuse-y (atom 0))
(def diffuse-z (atom 1))

(def light (atom (vec/normalize (Vector3D. -0.25 -0.75 1))))