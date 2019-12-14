(ns computer_graphics_coursework_backend.core
  (:require [computer_graphics_coursework_backend.ui.display :as display]
            [computer_graphics_coursework_backend.world.world :as world]
            [computer_graphics_coursework_backend.render.drawer :as drawer])
  (:import (computer_graphics_coursework_backend.render.vertex Vertex)
           (computer_graphics_coursework_backend.math.vector Vector4D)))

(defn -main []
  "Entry point for entire program"
  (world/generate-world (display/display-setup)))

