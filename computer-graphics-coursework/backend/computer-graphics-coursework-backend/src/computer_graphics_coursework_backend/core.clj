(ns computer_graphics_coursework_backend.core
  (:require [computer_graphics_coursework_backend.ui.display :as display]
            [computer_graphics_coursework_backend.world.world :as world]))

(defn -main []
  "Entry point for entire program"

  (world/generate-world (display/display-setup)))

