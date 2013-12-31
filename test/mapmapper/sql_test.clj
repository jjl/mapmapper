(ns mapmapper.sql_test
  (:require [clojure.test :refer :all]
            [mapmapper.sql :as s]))

(deftest utilities
  (testing "-generate-placeholdes"
    (let [g s/-generate-placeholders
          t0 (g 0)
          t1 (g 4)
          t2 (g 7)
          e0 ""
          e1 "? ? ? ?"
          e2 "? ? ? ? ? ? ?"]
      (is (= t0 e0))
      (is (= t1 e1))
      (is (= t2 e2)))))

(deftest insert
  (let [t1 (s/insert "foo" ["bar" "baz" "quux"])]
    (testing "map generation"
      (let [e1 {:type :insert :table "foo" :cols ["bar" "baz" "quux"]}]
        (is (= t1 e1))))
    (testing "sql generation"
      (let [s1 (s/generate t1)
            e1 "INSERT INTO foo ( \"foo\".\"bar\", \"foo\".\"baz\", \"foo\".\"quux\" ) VALUES ( ? ? ? )"]
        (is (= s1 e1))))))
