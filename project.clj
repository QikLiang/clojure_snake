(defproject snake "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [seesaw "1.5.0"]
                 [org.clojure/core.async "0.4.474"]
                 ; [fx-clj "0.2.0-alpha1"]
                 ]
  :main ^:skip-aot snake.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
