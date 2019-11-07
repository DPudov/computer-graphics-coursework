(ns computer-graphics-coursework-backend.core
  (:require [computer-graphics-coursework-backend.model :as model]
            [computer-graphics-coursework-backend.image :as image])
  (:import (java.awt Color)
       (javax.swing JFrame JPanel))
  (:gen-class))

(def model-resource "resources/african_head.obj")

(def texture-resource "resources/african_head_diffuse.png")

(defn render [model-file texture-file width height]
  (let [img (model/render model-file texture-file width height)]
    (proxy [JPanel] []
      (paintComponent [graphics]
        (proxy-super paintComponent graphics)
        (doto graphics
          (.drawImage img 0 0 nil))))))

(defn init [width height]
  (let [panel (doto (render model-resource
                            texture-resource
                            width
                            height)
                (.setOpaque true)
                (.setBackground Color/black))]
    (doto (JFrame. "Курсовая работа по КГ. Пудов.")
      ;;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setContentPane panel)
      (.setSize width (+ height 30))
      (.setVisible true))))

(defn -main
  "Entry point for the whole program."
  [& args]
  (time (init 800 800)))
