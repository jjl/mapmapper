(defproject mapmapper "0.1.0-SNAPSHOT"
  :description "MapMapper - Mapping database to userspace"
  :url "http://www.reposaurus.com/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options {:init-ns mapmapper.repl}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [korma "0.3.0-RC6"]
                 [com.taoensso/timbre "2.7.1"]])
