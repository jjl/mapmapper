(defproject mapmapper "0.1.0-SNAPSHOT"
  :description "MapMapper - Mapping database to userspace"
  :url "http://www.reposaurus.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options {:init-ns mapmapper.repl}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [korma "0.3.0-RC6"]
                 [org.flatland/ordered "1.5.2"]
                 [com.taoensso/timbre "2.7.1"]]
  :plugins [[lein-midje "3.1.3"]]
  :profiles {:dev {:dependencies [[midje "1.6.0"]]}})
