(ns computer_graphics_coursework_backend.world.world
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [computer_graphics_coursework_backend.render.camera :as camera]
            [computer_graphics_coursework_backend.render.drawer :as drawer]
            [computer_graphics_coursework_backend.world.terrain :as terrain]
            [computer_graphics_coursework_backend.world.water :as water]
            [computer-graphics-coursework-backend.world.config :as conf])
  (:import (java.awt.image BufferedImage)
           (java.awt Color Graphics2D)
           (javax.swing Timer JPanel)
           (java.awt.event ActionListener KeyEvent)))

(def desired-fps 1)

(defn time-ms-by-fps
  [fps]
  (/ 1000 fps))

(defn create-timer
  [tick-time canvas]
  (let []
    (Timer. tick-time (reify ActionListener
                        (actionPerformed [this e]
                          (let [[new-water new-energy]
                                (water/update-water (terrain/init-terrain-memo conf/dim) @water/water-map @water/energy-map conf/dim)]
                            (reset! water/water-map new-water)
                            (reset! water/energy-map new-energy))
                          (.repaint canvas))))))

(defn clear [canvas]
  (let [g (.getGraphics canvas)
        w (.getWidth canvas)
        h (.getHeight canvas)]
    (.setBackground g Color/WHITE)
    (.clearRect g 0 0 w h)))

(defn paint-frame [canvas ^Graphics2D graphics]
  (time (let [w (.getWidth canvas)
              h (.getHeight canvas)
              frame (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
              terrain-map (terrain/init-terrain-memo conf/dim)
              water-map @water/water-map
              terrain (terrain/get-terrain-voxels-memo conf/dim)
              water (water/get-water-voxels conf/dim terrain-map water-map Color/BLUE)
              voxels (vec (concat water terrain))]
          (println "Camera" @camera/cam)
          (println "Water" (count water) "Terrain" (count terrain))
          (drawer/draw-voxels frame voxels @camera/cam)
          (.drawImage graphics frame nil nil))))

(defn generate-world [root]
  (let [^JPanel canvas (select root [:#canvas])
        timer (create-timer (time-ms-by-fps desired-fps) canvas)]

    (.start timer)
    (-> canvas
        (request-focus!)
        (config! :paint paint-frame)
        (listen :key-pressed
                (fn [e]
                  (let [key (.getKeyCode e)]
                    (cond
                      (= key KeyEvent/VK_W)
                      (swap! camera/cam camera/move-forward 1 1)
                      (= key KeyEvent/VK_A)
                      (swap! camera/cam camera/move-left 1 1)
                      (= key KeyEvent/VK_S)
                      (swap! camera/cam camera/move-backward 1 1)
                      (= key KeyEvent/VK_D)
                      (swap! camera/cam camera/move-right 1 1)
                      (or (= key KeyEvent/VK_KP_UP) (= key KeyEvent/VK_UP))
                      (swap! camera/cam camera/turn-up 1 1)
                      (or (= key KeyEvent/VK_KP_DOWN) (= key KeyEvent/VK_DOWN))
                      (swap! camera/cam camera/turn-down 1 1)
                      (or (= key KeyEvent/VK_KP_RIGHT) (= key KeyEvent/VK_RIGHT))
                      (swap! camera/cam camera/turn-right 1 1)
                      (or (= key KeyEvent/VK_KP_LEFT) (= key KeyEvent/VK_LEFT))
                      (swap! camera/cam camera/turn-left 1 1)))
                  (.repaint canvas))))))

