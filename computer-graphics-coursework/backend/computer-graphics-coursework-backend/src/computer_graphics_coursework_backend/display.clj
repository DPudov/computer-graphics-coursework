(ns computer_graphics_coursework_backend.display
  (:import [javax.swing JFrame JLabel JButton JTextField JPanel])
  (:use seesaw.core
        seesaw.graphics
        seesaw.color))

(def title "Курсовая по компьютерной графике. Пудов Д.Ю.")

(defn a-sketch [e]
  (println "selecting animation"))

(defn a-help [e]
  (println "help..."))

(defn a-about [e]
  (println "about..."))

(def menus
  (let [a-sketch (action :handler a-sketch :name "Открыть" :tip "Выбор анимации..." :key "menu N")
        a-help (action :handler a-help :name "Помощь" :tip "Возможные действия в программе" :key "menu H")
        a-about (action :handler a-about :name "Разработчик" :tip "Информация о разработчике и версии" :key "menu A")]
    (menubar :items [(menu :text "Анимация..." :items [a-sketch])
                     (menu :text "Помощь" :items [a-help])
                     (menu :text "О приложении" :items [a-about])])))


(defn add-menu [frame]
  (config! frame :menubar menus))

(defn create-display-frame
  ([] (create-display-frame 1300 700))
  ([width height] (frame :id 1
                         :title title
                         :on-close :exit
                         :width width
                         :height height
                         :minimum-size [640 :by 480])))



(defn display-setup [width height]
  (invoke-later
    (-> (create-display-frame width height)
        add-menu
        show!)))


