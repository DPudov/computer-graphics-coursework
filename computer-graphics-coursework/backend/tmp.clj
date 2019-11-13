; Дерево-фрактал
(import '(javax.swing JFrame JPanel)
        '(java.awt Color Graphics Graphics2D))

(defn ^:static draw-tree [ #^Graphics g2d ^double angle ^double x ^double y ^double length ^double branch-angle ^long depth]
  (if (> depth 0)
    (let [new-x (- x (* length (Math/sin (Math/toRadians angle))))
          new-y (- y (*  length (Math/cos (Math/toRadians angle))))
          new-length (fn [] (* length (+ 0.75 (rand 0.1))))
          new-angle  (fn [op] (op angle (* branch-angle (+ 0.75 (rand)))))]
      (. g2d drawLine x y new-x new-y)
      (draw-tree g2d (new-angle +) new-x new-y (new-length) branch-angle (- depth 1))
      (draw-tree g2d (new-angle -) new-x new-y (new-length) branch-angle (- depth 1)))))

(defn render [ #^Graphics g w h]
  (doto g
    (.setColor (Color/BLACK))
    (.fillRect 0 0 w h)
    (.setColor (Color/GREEN)))
  (let [init-length ( / (min w h) 5),
        branch-angle (* 10 (/ w h)),
        max-depth 12]
    (#'draw-tree  g 0.0 (/ w 2) h init-length branch-angle max-depth)))

(defn create-panel []
    "Create a panel with a customised render"
  (proxy [JPanel] []
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (time (render g (. this getWidth) (. this getHeight))))))


(defn run []
  (let [frame (JFrame. "Clojure Fractal Tree")
        panel (create-panel)]
    (doto frame
      (.add panel)
      (.setSize 640 400)
      (.setVisible true))))

(run)


;  (:require [fn-fx.fx-dom :as dom]
;            [fn-fx.diff :refer [component defui render should-update?]]
;            [fn-fx.controls :as ui]))
;
;
;(def red (ui/color :red 1 :blue 0 :green 0))
;(def green (ui/color :red 0 :blue 0 :green 1))
;(def blue (ui/color :red 0 :blue 1 :green 0))
;(def grey (ui/color :red 0.5 :blue 0.5 :green 0.5))
;
;(def color-mapping
;  {:box      red
;   :sphere   green
;   :cylinder blue})
;
;(defn material-for-state [highlights shape]
;  (ui/phong-material
;    :diffuse-color (if (highlights shape)
;                     (color-mapping shape)
;                     grey)))
;
;(defui MainWindow
;       (render [this {:keys [highlights rotate-x]}]
;               (ui/group
;                 :rotate rotate-x
;                 :children [(ui/box :width 100 :height 100 :depth 100
;                                    :on-mouse-entered {:event :enter :shape :box}
;                                    :on-mouse-exited {:event :exit :shape :box}
;                                    :material (material-for-state highlights :box)
;                                    :translate-x 150 :translate-y 0 :translate-z 400)
;                            (ui/sphere :radius 50
;                                       :on-mouse-entered {:event :enter :shape :sphere}
;                                       :on-mouse-exited {:event :exit :shape :sphere}
;                                       :material (material-for-state highlights :sphere)
;                                       :translate-x 300 :translate-y -5 :translate-z 600)
;                            (ui/cylinder :radius 40 :height 120
;                                         :on-mouse-entered {:event :enter :shape :cylinder}
;                                         :on-mouse-exited {:event :exit :shape :cylinder}
;                                         :material (material-for-state highlights :cylinder)
;                                         :translate-x 500 :translate-y -25 :translate-z 600)
;                            (ui/point-light :translate-x 350 :translate-y 100 :translate-z 300)])))
;
;(defui Stage
;       (render [this args]
;               (ui/stage
;                 :title "Курсовая по компьютерной графике. Пудов."
;                 :shown true
;                 :scene (ui/scene
;                          :width 400
;                          :height 400
;                          :depth-buffer true
;                          :anti-aliasing javafx.scene.SceneAntialiasing/BALANCED
;                          :on-mouse-dragged {:event         :drag
;                                             :fn-fx/include {:fn-fx/event #{:scene-x :scene-y}}}
;                          :on-mouse-pressed {:event         :start-drag
;                                             :fn-fx/include {:fn-fx/event #{:scene-x :scene-y}}}
;                          :camera (ui/perspective-camera
;                                    :fixed-eye-at-camera-zero false
;                                    :translate-x 100 :translate-y -150 :translate-z 300)
;                          :root (main-window :highlights (:highlights args)
;                                             :rotate-x (:rotate-x args))))))
;
;(defmulti handle-event (fn [state {:keys [event]}]
;                         event))
;
;(defmethod handle-event :enter
;  [state {:keys [shape]}]
;  (update-in state [:highlights] conj shape))
;
;(defmethod handle-event :exit
;  [state {:keys [shape]}]
;  (update-in state [:highlights] disj shape))
;
;(defmethod handle-event :drag
;  [{:keys [drag] :as state} {:keys [fn-fx/includes]}]
;  (let [{:keys [scene-x scene-y]} (:fn-fx/event includes)
;        delta-x (- (:x drag) scene-x)]
;    (-> state
;        (update-in [:rotate-x] + delta-x)
;        (assoc-in [:drag] {:x scene-x :y scene-y}))))
;
;(defmethod handle-event :start-drag
;  [state {:keys [fn-fx/includes]}]
;  (let [{:keys [scene-x scene-y]} (:fn-fx/event includes)]
;    (assoc-in state [:drag] {:x scene-x :y scene-y})))
;
;(defn -main []
;  (let [;; Data State holds the business logic of our app
;        data-state (atom {:highlights #{}
;                          :drag       [0 0]
;                          :rotate-x   5})
;
;        ;; handler-fn handles events from the ui and updates the data state
;        handler-fn (fn [event]
;                     (try
;                       (swap! data-state handle-event event)
;                       (catch Throwable ex
;                         (println ex))))
;
;        ;; ui-state holds the most recent state of the ui
;        ui-state (agent (dom/app (stage @data-state) handler-fn))]
;
;    ;; Every time the data-state changes, queue up an update of the UI
;    (add-watch data-state :ui (fn [_ _ _ _]
;                                (send ui-state
;                                      (fn [old-ui]
;                                        (try
;                                          (dom/update-app old-ui (stage @data-state))
;                                          (catch Throwable ex
;                                            (println ex)))))))))