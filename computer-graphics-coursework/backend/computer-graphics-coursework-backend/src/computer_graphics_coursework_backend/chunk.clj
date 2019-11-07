(ns computer-graphics-coursework-backend.chunk)

(defprotocol chunk-protocol
    (update [this delta-time])
    (render [this renderer]))

(def chunk-size 16)

(deftype chunk [voxels])
