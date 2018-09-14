(ns allalin.core-test
    (:require
     [cljs.test :refer-macros [deftest is testing]]
     [allalin.core]))

(deftest multiply-test
  (is (= (* 1 2) (* 1 2))))

(deftest multiply-test-2
  (is (= (* 75 10) (* 10 75))))
