(ns computer-graphics-coursework-backend.core
  (:require [computer-graphics-coursework-backend.model :as model]
            [computer-graphics-coursework-backend.image :as image])
  (:import (java.awt Color)
           (javax.swing JFrame JPanel JOptionPane)
           (java.awt.event KeyListener KeyAdapter KeyEvent))
  (:gen-class))

(def model-resource "resources/african_head.obj")

(def texture-resource "resources/african_head_diffuse.png")


(defn add-key-listener
  [component f & args]
  (let [listener (proxy [KeyAdapter] []
                   (keyPressed [event]
                     (apply f event args)))]
    (.addKeyListener component listener)
    listener))

(defn key-listener
  [event]
  (case (.getKeyCode event)
    KeyEvent/VK_RIGHT (print "Right")
    KeyEvent/VK_LEFT (print "Left")
    (JOptionPane/showMessageDialog (.getSource event) "default case")))

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
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setContentPane panel)
      (.setSize width (+ height 30))
      (.setVisible true))
    (.setFocusable panel true)
    (add-key-listener panel key-listener)))

(defn -main
  "Entry point for the whole program."
  [& args]
  (time (init 800 800)))
