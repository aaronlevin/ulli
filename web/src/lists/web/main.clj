(ns lists.web.main
  "Entry-point for launching the web app."
  (:require
    [clojure.string :as s]
    [clojure.pprint :as pprint]
    [lists.web.db :as db]
    [lists.web.handler :as h]
    [lists.web.config :as config]
    [lists.web.util :refer [exit str->int default-postgres-port]]
    [lists.clj-log.log :as log]
    [ring.adapter.jetty :refer [run-jetty]])
  (:import
    org.eclipse.jetty.server.Server))

(defn system
  "Return a map of all the components needed to start the system.  config is a
  map of configuration values.  Note that the system is not yet started."
  [config]
  {:config config
   :datasource (db/datasource (:db-host config)
                              (:db-port config)
                              (:db-user config)
                              (:db-pass config)
                              (:db-name config))})

(defn start
  "Start all the components of the system.  Returns a new system."
  [system]
  (let [handler (h/app system)
        conf-fn #(.setStopAtShutdown ^Server % true)
        server (run-jetty handler
                          {:port (get-in system [:config :port])
                           :join? false
                           :max-threads 200
                           :configurator conf-fn})]
    (assoc system
           :handler handler
           :server server)))

(defn stop
  "Performs side-effects to stop the system and release its resources.  Returns
  an updated instance of system."
  [system]
  (when-let [^Server s (:server system)]
    (.stop s))
  (when-let [ds (:datasource system)]
    (db/close ds)))

(defn -main [& argv]
  (let [config (config/config)]
    (when (= "--help" (first argv))
      (pprint/pprint config)
      (exit 1 ""))
    (let [system (start (system config))]
      (log/info (str "started lists web app"))
      (.join ^Server (:server system)))))

