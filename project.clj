(defproject
  allalin "0.1.0-SNAPSHOT"
  :description "Presentation framework, built with Clojurescript, Rum and Figwheel"
  :url "https://artesonraju.github.io/allalin/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :min-lein-version "2.7.1"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.339"]
                 [org.clojure/core.async "0.4.474"]
                 [rum "0.11.2"]]
  :source-paths ["src"]
  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" allalin.test-runner]}
  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.1.9"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   :resource-paths ["target"]
                   ;; need to add the compliled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["target"]}})

