(ns computer_graphics_coursework_backend.voxel)

(map voxel-types [:default
                  :water
                  :stone
                  :sand :grass
                  :dirt
                  :wood] (range))


(defprotocol voxel-protocol
    (is-active [this])
    (set-active! [this value]))

; type that represents one voxel
(deftype voxel [^:volatile-mutable is-active voxel-type]
    voxel-protocol
    (is-active [this] (. this is-active))
    (set-active! [this value] (set! is-active val)))
