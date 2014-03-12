(ns lists.web.test-utils
  (:require
    [lists.web.config :as config]
    [lists.web.main :as main]
    [lists.clj-test-common.db :as db]))

(defmacro with-test-system
  "Create and start entire system with a private, empty db.  Binds the system
  to sym and excutes body in this context.  Tears the whole thing down at the
  end and returns nil."
  [sym & body]
  `(db/with-empty-db "lists/web/schema/lists.sql"
     (fn [dbname#]
       (let [config# (assoc (config/config)
                            :port (db/find-free-port)
                            :db-name dbname#)
             system# (main/system config#)
             ~sym (main/start system#)]
         (try
           ~@body
           (finally
             (main/stop ~sym)))))))

