(ns lists.clj-log.log
  "Small set of macros that form a simple wrapper over slf4j->logback."
  (:import
    [java.util TimeZone]
    [ch.qos.logback.classic Level]
    [ch.qos.logback.classic.filter ThresholdFilter]
    [ch.qos.logback.classic.encoder PatternLayoutEncoder]
    [ch.qos.logback.core ConsoleAppender]
    [org.slf4j Logger LoggerFactory]))

(def ^:dynamic *enabled*
  "If false, logging calls are not executed.  Useful in tests when you want
  quiet."
  true)

(defmacro suppress
  "Eval body with logging disabled."
  [& body]
  `(binding [*enabled* false] ~@body))

(def levels
  {:off Level/OFF
   :error Level/ERROR
   :warn Level/WARN
   :info Level/INFO
   :debug Level/DEBUG
   :trace Level/TRACE
   :all Level/ALL})

(def logger-levels
  "Map of logger names to desired log level.  Prefer calling update-config to
  change this.  Changing this atom does not make the config live, configure!
  must be called afterwards."
  (atom {"ROOT" :warn}))

(defn configure!
  "Apply current contents of logger-levels atom to to logback."
  []
  (let [config @logger-levels]
    (doseq [[logger-name level] config]
      (let [logger (LoggerFactory/getLogger logger-name)]
        (when-let [level (get levels level)]
          (.setLevel logger level))))))

(defn update-config!
  "Config is a map of logger name -> level keyword.  This function merges
  config into existing configuration and updates all the loggers."
  [config]
  (swap! logger-levels #(merge % config))
  (configure!))

(defn bootstrap!
  "Programatically configure logging, first throwing out any existing logging
  configuration that may already be present."
  []
  (let [^ch.qos.logback.classic.Logger root (LoggerFactory/getLogger Logger/ROOT_LOGGER_NAME)
        context (.getLoggerContext root)
        encoder (PatternLayoutEncoder.)
        appender (ConsoleAppender.)
        tz (.. (TimeZone/getDefault) (getID))]
    (.detachAndStopAllAppenders root)
    (.setLevel root (:off levels))
    (doto encoder
      (.setContext context)
      (.setPattern (str "%-5p [%d{yyyy-MM-dd'T'HH:mm:ss.SSS'Z',UTC}] %c: %m%n"))
      (.start))
    (doto appender
      (.setContext context)
      (.setEncoder encoder)
      (.start))
    (.addAppender root appender))
  (configure!))

;; Bootstrap as early as possible.  So long as this ns is loaded (required)
;; early, any 3rd party libs that log at class loading time (e.g. Jetty) will
;; go through our configured appender, and not spam some arbitrary log format.
(bootstrap!)

(defmacro get-logger []
  `(LoggerFactory/getLogger ~(name (ns-name *ns*))))

;; Considered macro-generating-macro for the below boilerplate, but decided to
;; err on the side of maintainability.

(defmacro trace
  ([^String msg]
   `(when *enabled* (.trace (get-logger) ~msg)))
  ([^String msg ^Throwable e]
   `(when *enabled* (.trace (get-logger) ~msg ~e))))

(defmacro debug
  ([^String msg]
   `(when *enabled* (.debug (get-logger) ~msg)))
  ([^String msg ^Throwable e]
   `(when *enabled* (.debug (get-logger) ~msg ~e))))

(defmacro info
  ([^String msg]
   `(when *enabled* (.info (get-logger) ~msg)))
  ([^String msg ^Throwable e]
   `(when *enabled* (.info (get-logger) ~msg ~e))))

(defmacro warn
  ([^String msg]
   `(when *enabled* (.warn (get-logger) ~msg)))
  ([^String msg ^Throwable e]
   `(when *enabled* (.warn (get-logger) ~msg ~e))))

(defmacro error
  ([^String msg]
   `(when *enabled* (.error (get-logger) ~msg)))
  ([^String msg ^Throwable e]
   `(when *enabled* (.error (get-logger) ~msg ~e))))

