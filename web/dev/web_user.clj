(ns web-user
  "Dev namespace supporting interactive development.  See
  http://thinkrelevance.com/blog/2013/06/04/clojure-workflow-reloaded for more
  explanation."
  (:require
    [clojure.repl :refer :all]
    [clojure.tools.namespace.repl :refer [disable-unload! refresh]]
    [clojure.pprint :refer [pprint pp]]
    [clojure.reflect :refer [reflect]]
    [clojure.test :refer [run-all-tests]]
    [clojure.walk :refer [macroexpand-all]]
    [net.cgrand.reload :refer [auto-reload]]
    lists.web.db
    lists.web.main))

;; Auto-reload enlive resources (HTML files) loaded in select namespaces
;; auto-reload is badly behaved, causes JVM to hang, working on it
;; https://github.com/overthink/enlive/commit/714867c8712366ae7d6210262eeac018975403c1
(when (get (System/getenv) "ENLIVERELOAD")
  (println "Starting badly behaved auto-reloader")
  (auto-reload (find-ns 'lists.web.web)))

;; Prevent this ns from being reloaded by `reset` below.  This prevents us
;; from "losing" this namespace due to syntax errors in other parts of the
;; app.  The cost is you can't have namespace alises in this file.  No (require
;; [foo :as somealias]).  This seems a small price to pay.
(disable-unload!)

;; Holds the current system instance. Used in dev mode only.  This is defonce
;; so we avoid losing its contents if the namespace is re-evaluated.
(defonce system nil)

(comment test)

(defn init
  "Constucts a new dev system and puts it in the system global var."
  []
  (alter-var-root #'system
                  (constantly (lists.web.main/system (lists.web.config/config)))))

(defn start
  "Starts the current dev system."
  []
  (alter-var-root #'system lists.web.main/start))

(defn stop
  "Shuts down and destroys the current dev system. Updates global system var."
  []
  (alter-var-root #'system
                  (fn [system] (when system (lists.web.main/stop system)))))

(defn go
  "Init the dev system and start it. Returns nil."
  []
  (init)
  (start))

(defn reset
  "Tear down current system (if any), reload all changed code, and restart
  system. Call this after changes to 'safely' reload all code and restart."
  []
  (stop)
  (refresh :after 'web-user/go))

;; Remove ref to clojure.core/test since we're about to redefine it.  This
;; prevents a warning on stdout.  :refer-clojure in ns macro doesn't work here
;; since the user ns is created before this file is loaded.
(ns-unmap 'web-user 'test)

(defn test
  "Run tests in all project nses not ending with -slow."
  []
  (refresh)
  (run-all-tests #"^lists\.web\..*-test(?!.*-slow)$"))

(defn test-all
  "Run all the tests."
  []
  (refresh)
  (run-all-tests #"^lists.web\..*-test.*"))

(defmacro with-conn
  "Bind a new db connection to conn-sym, then eval body in that context and
  call commit. e.g.  (with-conn c (db/my-thing-needing-connection c))."
  [conn-sym & body]
  `(with-open [~conn-sym (lists.web.db/conn (:datasource system))]
     ~@body
     (.commit ~conn-sym)))

