(ns lists.web.main-test-slow
  (:require
    [clojure.test :refer [deftest is testing]]
    [lists.web.test-utils :refer [with-test-system]]))

(deftest start-stop-sanity
  (testing "fire up the entire system, tear it down"
    (with-test-system s
      (is (:server s))
      (is (:handler s)))))

