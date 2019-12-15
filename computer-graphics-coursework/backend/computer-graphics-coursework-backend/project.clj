(defproject computer-graphics-coursework-backend "0.1.0-SNAPSHOT"
  :description "Pudov's computer graphics coursework"
  :url "https://github.com/DPudov"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [seesaw "1.5.0"]
                 [uncomplicate/neanderthal "0.26.1"]]
  :repl-options {:init-ns computer_graphics_coursework_backend.core}
  :main computer_graphics_coursework_backend.core
  :profiles {:uberjar {:aot :all}}
  :java-cmd "/usr/lib/jvm/java-1.11.0-openjdk-amd64/bin/java"
  :jvm-opts "--add-opens=java.base/jdk.internal.ref=ALL-UNNAMED")

