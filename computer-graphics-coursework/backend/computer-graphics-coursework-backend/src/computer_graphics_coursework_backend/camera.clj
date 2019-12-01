(ns computer_graphics_coursework_backend.camera)

(definterface Viewable
  (getPosition [])
  (getTarget []))

(deftype Camera
  [position target]
  Viewable
  (getPosition [_] position)
  (getTarget [_] target))
