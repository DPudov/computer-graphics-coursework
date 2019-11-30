(ns computer_graphics_coursework_backend.display
  (:use seesaw.core
        seesaw.graphics
        seesaw.color)
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def title "Курсовая по компьютерной графике. Пудов Д.Ю.")
(def default-background "#ffffff")
(def version "Версия")
(def developer "Разработчик")
(def help "Помощь")
(def open "Открыть")

(defn get-version []
  (some-> (io/resource "project.clj") slurp edn/read-string (nth 2)))

(defn a-sketch [e]
  (println "selecting animation"))

(defn a-help [e]
  (println "help..."))

(defn a-about-developer [e]
  (-> (dialog
        :title developer
        :content "Пудов Дмитрий Юрьевич, ИУ7-53Б"
        :type :info)
      pack!
      show!))

(defn a-about-version [e]
  (->
    (dialog
      :title version
      :content (get-version)
      :type :info)
    pack!
    show!))


(def menus
  "Define menus at menubar"
  (let [a-sketch (action :handler a-sketch
                         :name open
                         :tip "Выбор анимации..."
                         :key "menu N")
        a-help (action :handler a-help
                       :name help
                       :tip "Возможные действия в программе"
                       :key "menu H")
        a-about-developer (action :handler a-about-developer
                                  :name developer
                                  :tip "Информация о разработчике"
                                  :key "menu A")
        a-about-version (action :handler a-about-version
                                :name version
                                :tip "Информация о версии"
                                :key "menu shift V")]

    (menubar :items [(menu :text "Анимация..." :items [a-sketch])
                     (menu :text help :items [a-help])
                     (menu :text "О приложении" :items [a-about-developer a-about-version])])))


(defn add-menu [frame]
  (config! frame :menubar menus))

(defn create-canvas [background]
  (canvas :id :canvas
          :background background
          :paint nil))


(defn create-control-panel []
  (vertical-panel :items [
                          (horizontal-panel :items [(vertical-panel :items ["dx" (text)])
                                                    (vertical-panel :items ["dy" (text)])
                                                    (vertical-panel :items ["dz" (text)])])
                          (button :text "Переместить камеру")
                          (horizontal-panel :items [(vertical-panel :items ["kx" (text)])
                                                    (vertical-panel :items ["ky" (text)])
                                                    (vertical-panel :items ["kz" (text)])])
                          (button :text "Масштабировать")
                          (horizontal-panel :items [(vertical-panel :items ["ox" (text)])
                                                    (vertical-panel :items ["oy" (text)])
                                                    (vertical-panel :items ["oz" (text)])])
                          (button :text "Повернуть камеру на N градусов")
                          (horizontal-panel :items [(vertical-panel :items ["x" (text)])
                                                    (vertical-panel :items ["y" (text)])
                                                    (vertical-panel :items ["z" (text)])])
                          (button :text "Изменить координаты источника света")
                          (horizontal-panel :items [(vertical-panel :items ["x" (text)])
                                                    (vertical-panel :items ["y" (text)])
                                                    (vertical-panel :items ["z" (text)])])
                          (button :text "Изменить координаты наблюдателя")]))


(defn create-content []
  (border-panel :hgap 10 :vgap 15 :border 5
                :center (create-canvas default-background)
                :east (create-control-panel)))

(defn create-display-frame
  ([content] (create-display-frame content 1300 700))
  ([content width height] (frame :id :display
                                 :title title
                                 :on-close :exit
                                 :width width
                                 :height height
                                 :content content
                                 :minimum-size [640 :by 480])))



(defn display-setup [width height]
  (invoke-later
    (-> (create-display-frame (create-content) width height)
        add-menu
        show!)))


