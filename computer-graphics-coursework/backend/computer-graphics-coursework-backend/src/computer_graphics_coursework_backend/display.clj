(ns computer_graphics_coursework_backend.display
  (:import [javax.swing JFrame JLabel JButton JTextField JPanel])
  (:use seesaw.core))

(def title "Курсовая по компьютерной графике. Пудов Д.Ю.")

(defn get-canvas
  [frame])


(defn create-canvas []
  "Create a panel with a customized render"
  (proxy [JPanel] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (println "Rendering..."))))

(defn create-menu
  [frame]
  (let [
        ; rotate section
        phi-ox-label (JLabel. "OX, градусов")
        phi-oy-label (JLabel. "OY, градусов")
        phi-oz-label (JLabel. "OZ, градусов")
        phi-ox-edit (JTextField. 20)
        phi-oy-edit (JTextField. 20)
        phi-oz-edit (JTextField. 20)
        rotate-button (JButton. "Повернуть")
        rotate-panel (JPanel.)

        ; translate section
        dx-label (JLabel. "dx")
        dy-label (JLabel. "dy")
        dz-label (JLabel. "dz")
        dx-edit (JTextField. 20)
        dy-edit (JTextField. 20)
        dz-edit (JTextField. 20)
        translate-button (JButton. "Переместить камеру")
        translate-panel (JPanel.)

        ; scale section
        kx-label (JLabel. "kx")
        ky-label (JLabel. "ky")
        kz-label (JLabel. "kz")
        kx-edit (JTextField. 20)
        ky-edit (JTextField. 20)
        kz-edit (JTextField. 20)
        scale-button (JButton. "Масштабировать")
        scale-panel (JPanel.)

        ; graphics section

        graphics-panel (create-canvas)

        ; main section
        main-panel (JPanel.)]

    (doto rotate-panel
      (.add phi-ox-label)
      (.add phi-oy-label)
      (.add phi-oz-label)
      (.add phi-ox-edit)
      (.add phi-oy-edit)
      (.add phi-oz-edit)
      (.add rotate-button))
    (doto translate-panel
      (.add dx-label)
      (.add dy-label)
      (.add dz-label)
      (.add dx-edit)
      (.add dy-edit)
      (.add dz-edit)
      (.add translate-button))
    (doto scale-panel
      (.add kx-label)
      (.add ky-label)
      (.add kz-label)
      (.add kx-edit)
      (.add ky-edit)
      (.add kz-edit)
      (.add scale-button))
    (doto main-panel
      (.add graphics-panel)
      (.add rotate-panel)
      (.add translate-panel)
      (.add scale-panel))
    (doto frame
      (.add main-panel))))
;
(defn display-setup []
  (let [frame (JFrame. (str title))]
    (doto frame
      (create-menu)
      (.setExtendedState JFrame/MAXIMIZED_BOTH)
      (.setResizable false)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setVisible true))))



;
; (defn display-destroy
;     [])
;
; (defn display-update
;     [])
