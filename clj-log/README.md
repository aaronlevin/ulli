# clj-log

Some Clojure macros over slf4j and logback.

## Usage

All logging is disabled by default.  Config is programatic only (no magic property files). e.g.

    (require '[lists.clj-log.log :as log]])
    (log/update-config! {"my-ns" :debug})
    (in-ns my-ns)
    (log/debug "some debug logging")
    (log/trace "won't be shown")

