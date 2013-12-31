(ns mapmapper.sql_test
  (:require [clojure.test :refer :all]
            [mapmapper.sql :as s]))

(deftest utilities
  (testing "-generate-placeholders"
    (let [g s/-generate-placeholders
          t0 (g 0)
          t1 (g 1)
          t2 (g 4)
          e0 ""
          e1 "?"
          e2 "?, ?, ?, ?"]
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
            e1 "INSERT INTO foo ( \"foo\".\"bar\", \"foo\".\"baz\", \"foo\".\"quux\" ) VALUES ( ?, ?, ? )"]
        (is (= s1 e1))))))

(deftest select
  (let [t1 (s/select "foo")]
    (testing "map generation"
      (let [e1 {:type :select :table "foo"}]
        (is (= t1 e1))))
    (testing "sql generation"
      (let [s1 (s/generate t1)
            e1 "not implemented"]
        (is (= s1 e1))))))

(deftest update
  (let [t1 (s/update "foo")]
    (testing "map generation"
      (let [e1 {:type :update :table "foo"}]
        (is (= t1 e1))))
    (testing "sql generation"
      (let [s1 (s/generate t1)
            e1 "not implemented"]
        (is (= s1 e1))))))

(deftest delete
  (let [t1 (s/delete "foo")]
    (testing "map generation"
      (let [e1 {:type :delete :table "foo"}
            t2 (s/generate {:type :delete
                            :table "foo"
                            :where [:op "=" [[:identifier "bar"] [:value 1]]]})]
        (is (= t1 e1))
        (is (= t2 "DELETE FROM foo WHERE (\"bar\" = 1)"))))
    (testing "sql generation"
      (let [s1 (s/generate t1)
            e1 "DELETE FROM foo"]
        (is (= s1 e1))))))
