(ns lists.web.util
  (:require
    [clojure.string :as s])
  (:import
    [java.text DecimalFormat NumberFormat SimpleDateFormat]
    org.mindrot.jbcrypt.BCrypt))

(defn run-mode []
  (s/lower-case (System/getProperty "run.mode" "")))

(defn production?
  "True if running in production, false otherwise."
  []
  (= (run-mode) "production"))

(defn dev-mode?
  []
  (not (production?)))

(defn str->int [^String s] (Integer/valueOf s))

(defn default-postgres-port
  "Get the default port that PostgreSQL is expected to be listening on for
  localhost.  Uses env var PG_PORT if set."
  []
  (str->int (get (System/getenv) "PG_PORT" "5432")))

(defn encrypt-pw
  "Encrypt cleartext with bcrypt.  Uses an auto-generated salt.  Returns a
  string that can be checked with check-pw."
  ([^String cleartext]
   (encrypt-pw cleartext 12))
  ([^String cleartext work-factor]
   (BCrypt/hashpw cleartext (BCrypt/gensalt work-factor))))

(defn check-pw
  "Check if candidate is a match for hashed."
  [^String candidate ^String hashed]
  (BCrypt/checkpw candidate hashed))

(defn exit
  "Prints msg then exists with status."
  [status msg]
  (println msg)
  (System/exit status))

