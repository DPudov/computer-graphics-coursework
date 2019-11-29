(ns computer_graphics_coursework_backend.controller
    (:import [javax.swing JFrame JLabel JButton]))

(defn get-time
  "Current time in millis"
  []
  (System/currentTimeMillis))
;
;
; (defn process-input
;   "Process all of the user actions"
;   [world])
;
;
; (defn is-close-requested
;   "Is there an user interrupt?"
;   [])
