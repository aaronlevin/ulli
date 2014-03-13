(ns lists.web.db
  "Database connect and query functionality."
  (:require
    [clojure.string :as string]
    [cheshire.core :as json])
  (:import
    org.apache.commons.dbcp.BasicDataSource
    java.sql.Connection
    javax.sql.DataSource))

(defn ^DataSource datasource
  "Create a datasource for the app to get db connections from."
  [db-host db-port db-user db-pass db-name]
  (doto (BasicDataSource.)
    (.setDriverClassName "org.postgresql.Driver")
    (.setUsername db-user)
    (.setPassword db-pass)
    (.setUrl (format "jdbc:postgresql://%s:%d/%s" db-host db-port db-name))
    (.setDefaultAutoCommit false)
    (.setDefaultReadOnly false)
    (.setMaxActive 10)
    (.setMinIdle 0) ; allow all connections to close
    (.setMinEvictableIdleTimeMillis (* 30 60 1000))
    (.setValidationQuery "SELECT 1")
    (.setTestWhileIdle true)
    (.setTestOnBorrow false) ; avoid extra round-trips
    (.setTestOnReturn false)
    (.setTimeBetweenEvictionRunsMillis (* 30 1000))))

(defn ^Connection conn
  "Helper to get a conection from a datasource."
  [^DataSource ds] (.getConnection ds))

(defn close
  "Close the given datasource.  Returns nil."
  [^BasicDataSource datasource]
  (.close datasource))

(defn health-check
  "Sanity check db connectivity.  Returns logical true if all good, throws
  otherwise."
  [^Connection conn]
  (let [q "SELECT 1 FROM account LIMIT 1"]
    (with-open [stmt (.prepareStatement conn q)]
      (doall (first (resultset-seq (.executeQuery stmt)))))))

