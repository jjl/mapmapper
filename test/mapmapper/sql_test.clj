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
  (let [t1 (s/delete "foo")
        t2 (-> (s/delete "foo")
               (s/where [:value true]))
        t3 (-> (s/delete "foo")
               (s/where [:op "and" [[:value true] [:value false]]]))
        t4 (-> (s/delete "foo")
               (s/where [:op "=" [[:value 1] [:identifier ["table" "column"]]]]))]
    (testing "map generation"
      (let [e1 {:type :delete :table "foo"}
            e2 {:type :delete :table "foo" :where [:value true]}
            e3 {:type :delete :table "foo" :where [:op "and"
                                                   [[:value true]
                                                    [:value false]]]}
            e4 {:type :delete :table "foo" :where [:op "="
                                                   [[:value 1]
                                                    [:identifier ["table" "column"]]]]}]
        (is (= t1 e1))
        (is (= t2 e2))
        (is (= t3 e3))
        (is (= t4 e4))
    (testing "sql generation"
      (let [s1 (s/generate t1)
            s2 (s/generate t2)
            s3 (s/generate t3)
            s4 (s/generate t4)
            e1 "DELETE FROM foo"
            e2 "DELETE FROM foo WHERE true"
            e3 "DELETE FROM foo WHERE (true and false)"
            e4 "DELETE FROM foo WHERE (1 = \"table\".\"column\")"]
        (is (= s1 e1))
        (is (= s2 e2))
        (is (= s3 e3))
        (is (= s4 e4))))))))
