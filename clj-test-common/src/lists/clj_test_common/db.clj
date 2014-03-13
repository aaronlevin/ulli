(ns lists.clj-test-common.db
  "Scaffolding for testing databases."
  (:require
    [clojure.java.shell :refer [sh]]
    [clojure.string :as s])
  (:import
    java.io.File
    java.net.ServerSocket
    [java.sql Connection Statement]
    [javax.sql DataSource]))

(defn- str->int [^String s] (Integer/valueOf s))

(defn default-postgres-port
  "Get the default port that PostgreSQL is expected to be listening on for
  localhost.  Uses env var PG_PORT if set."
  []
  (str->int (get (System/getenv) "PG_PORT" "5432")))

;; Make this ^:dynamic if we ever have a situation where the test db is not on
;; localhost.
(def test-db-info
  "Connection params for the postgres instance that will be used for tests."
  {:host "localhost"
   :port (default-postgres-port)
   :admin-user "postgres"})

(defn psql
  "Execute psql command as postgres admin user.
  e.g. (psql \"template1\" \"-c\" \"CREATE DATABASE foo_db\")
       (psql \"foo_db\" \"-f\" \"/some/file.sql\")
  If psql returns non-zero, an exception is thrown, otherwise the full result
  is returned wiht these keys:
    :exit - Exit code
    :out  - Lines on stdout
    :err  - Lines on stderr"
  [dbname & args]
  (let [cmd-prefix ["psql"
                    "--no-psqlrc"
                    "-v" "ON_ERROR_STOP=on"
                    "--quiet"
                    "-h" (:host test-db-info)
                    "-p" (str (:port test-db-info))
                    "-d" dbname
                    "-U" (:admin-user test-db-info)]
        full-cmd (concat cmd-prefix args)]
    (let [result (apply sh full-cmd)]
      (when (not (zero? (:exit result)))
        (binding [*out* *err*]
          (println result))
        (throw (Exception.
                 (format "psql exited with a non-zero return code: %d, cmd: '%s', err: '%s'"
                         (:exit result)
                         (vec full-cmd)
                         (:err result)))))
      result)))

(defn create-db
  "Create a database with the given name."
  [dbname]
  (psql "template1" "-c" (str "CREATE DATABASE " dbname)))

(defn drop-db
  "Remove database with given name."
  [dbname]
  (psql "template1" "-c" (str "DROP DATABASE " dbname)))

(defn unique-name
  "Append _<random stuff> to name to make it unique (enough)."
  [name]
  (s/join "_" [name (System/currentTimeMillis) (rand-int 1000000)]))

(defn ^File find-file
  "Search for path starting at File(\".\") and moving upwards towards the fs
  root.  If x/path is found, return File(x), otherwise return nil.  This is
  most useful if you specify a filename relative to the git repo root, e.g.
  (find-file \"web-proj/schema/foo.sql\")"
  [^String path]
    (loop [dir (.getCanonicalFile (File. "."))]
      (when dir
        (let [target (File. dir path)]
          (if (.exists target)
            (.getCanonicalFile target)
            (recur (.getParentFile dir)))))))

(defn find-free-port
  "Find a free port to listen on.  We don't just use the default in order to
  allow multiple processes to run tests at the same time, i.e. on the build
  servers."
  []
  (with-open [ss (ServerSocket. 0)]
    (.getLocalPort ss)))

(defn with-empty-db
  "Create a new empty db and load schema at schema-path into it.  Call f with
  the name of the db created.  After f completes, destroy the database."
  [^String schema f]
  (let [test-dbname (unique-name "test_db")
        schema0 (.getCanonicalPath (find-file schema))]
    (create-db test-dbname)
    (try
      (psql test-dbname "-f" schema0)
      (f test-dbname)
      (finally
        (drop-db test-dbname)))))

(defn query
  "Execute query against db, return results as a seq of maps via
  resultset-seq. Connection is rolled back."
  [^DataSource ds sql]
  (with-open [^Connection c (.getConnection ds)
              ^Statement stmt (.createStatement c)
              rs (.executeQuery stmt sql)]
    (let [results (doall (resultset-seq rs))]
      (.rollback c)
      results)))

