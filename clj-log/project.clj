(defproject lists/clj-log "1.0-SNAPSHOT"
  :description "Simple logging module for use from Clojure."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.slf4j/slf4j-api "1.7.5"]
                 [ch.qos.logback/logback-core "1.0.13"]
                 [ch.qos.logback/logback-classic "1.0.13"]]
  :global-vars {*warn-on-reflection* true})
