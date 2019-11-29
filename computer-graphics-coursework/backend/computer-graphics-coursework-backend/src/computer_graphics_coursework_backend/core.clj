(ns computer_graphics_coursework_backend.core
  (:require [computer_graphics_coursework_backend.controller :as controls]
            [computer_graphics_coursework_backend.display :as display]
            [computer_graphics_coursework_backend.world :as world]))


(def max-fps 60)

(def window-width 800)
(def window-height 800)

; (defn main-loop
;   "Main pipeline of graphics engine"
;   [initial-state]
;   (loop [initial-timestamp (controls/get-time)
;          accumulated-time 0
;          state initial-state]
;     (if (or (controls/is-close-requested)
;             (:exit state))
;       (display/display-destroy)
;       (let [new-timestamp (controls/get-time)
;             delta-time (+ (- new-timestamp initial-timestamp) accumulated-time)
;             [new-state remaining-time] (world/update-world state delta-time)
;             final-state (-> new-state controls/process-input world/render-world)]
;         (display/display-update)
;         (recur new-timestamp remaining-time final-state)))))
;
;
; (defn startup
;   "Returns initial state for world and starts window"
;   [world]
;   (let [world (world/fulfill-world world)]
;     (display/display-setup world)
;     (main-loop world)))


(defn -main []
  "Entry point for entire program"
  (display/display-setup 800 600)
  (world/generate-world window-width window-height))

