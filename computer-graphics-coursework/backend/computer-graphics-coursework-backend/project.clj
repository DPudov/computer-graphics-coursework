(defproject computer-graphics-coursework-backend "0.1.0-SNAPSHOT"
  :description "Pudov's computer graphics coursework"
  :url "https://github.com/DPudov"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [fn-fx/fn-fx-openjfx11 "0.5.0-SNAPSHOT"]
                 [uncomplicate/clojurecl "0.13.0"]]
  :repl-options {:init-ns computer-graphics-coursework-backend.core}
  :main computer-graphics-coursework-backend.core
  :profiles {:uberjar {:aot :all}}
  :java-cmd "/usr/lib/jvm/java-1.11.0-openjdk-amd64/bin/java")
