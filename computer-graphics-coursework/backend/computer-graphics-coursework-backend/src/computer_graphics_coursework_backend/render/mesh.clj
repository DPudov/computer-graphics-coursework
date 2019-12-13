(ns computer_graphics_coursework_backend.render.mesh)

(defrecord Mesh [triangles lines])

(defn add-mesh
  [first-mesh second-mesh]
  (Mesh. (vec (concat (:triangles first-mesh) (:triangles second-mesh)))
         (vec (concat (:lines first-mesh) (:lines second-mesh)))))


