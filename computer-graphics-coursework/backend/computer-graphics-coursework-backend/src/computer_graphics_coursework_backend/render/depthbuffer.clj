(ns computer_graphics_coursework_backend.render.depthbuffer)

(defn clear-depth-buffer
  [depth-buffer]
  (vec (for [x depth-buffer
             :let [cleared-value Double/MAX_VALUE]]
         cleared-value)))

