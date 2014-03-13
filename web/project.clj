(defproject web "1.0-SNAPSHOT"
  :description "Make some lists"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;;[org.clojure/tools.cli "0.3.1"]
                 [joda-time "2.3"]
                 [ring "1.2.1"]
                 [compojure "1.1.6"]
                 [enlive "1.1.5"]
                 [cheshire "5.2.0"]
                 [commons-dbcp "1.4"]
                 [postgresql "9.1-901.jdbc4"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [lists/clj-log "1.0-SNAPSHOT"]]
  :global-vars {*warn-on-reflection* false}
  :repl-options {:init-ns web-user}
  :profiles {:dev {:source-paths ["dev"]
                   :dependencies [[org.clojure/tools.namespace "0.2.4"]
                                  [lists/clj-test-common "1.0-SNAPSHOT"]
                                  [ring-mock "0.1.5"]]}})

