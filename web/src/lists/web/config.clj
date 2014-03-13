(ns lists.web.config
  "Eventually may have logic to find config and provide it.  For now, this *is*
  the config.")

(defn config
  "Provides a map of config for the app."
  []
  {:port 5001
   :db-host "localhost"
   :db-port 5432
   :db-user "postgres"
   :db-pass ""
   :db-name "lists"})

