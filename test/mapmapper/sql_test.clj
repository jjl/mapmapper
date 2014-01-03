(ns mapmapper.sql_test
  (:require [clojure.test :refer :all]
            [mapmapper.sql :as s]))

(deftest insert
  (let [t1 (s/fields (s/insert "foo") ["bar" "baz" "quux"])]
    (testing "map generation"
      (let [e1 {:type :insert :table "foo"
                :fields [[[:identifier ["bar"]] [:placeholder]]
                         [[:identifier ["baz"]] [:placeholder]]
                         [[:identifier ["quux"]] [:placeholder]]]}]
        (is (= t1 e1))))
    (testing "sql generation"
      (let [s1 (s/render t1)
            e1 "INSERT INTO foo ( \"bar\", \"baz\", \"quux\" ) VALUES ( ?, ?, ? )"]
        (is (= s1 e1))))))

(deftest select
  (let [t1 (-> (s/select "foo")
               (s/from [[:table "foo"]])
               (s/fields ["bar" "baz" "quux"]))]
    (testing "map generation"
      (let [e1 {:type :select :from [[:table "foo"]] :fields [[:identifier ["bar"]]
                                                              [:identifier ["baz"]]
                                                              [:identifier ["quux"]]]
                :meta {}}]
        (is (= t1 e1))))
    (testing "sql generation"
      (let [s1 (s/render t1)
            e1 "SELECT \"bar\", \"baz\", \"quux\" FROM \"foo\""]
        (is (= s1 e1))))))

(deftest update
  (let [insufficient-data #"^Expected collection of minimum length:"
        expected-vector #"^Expected vector:"
        gen-err (fn [x] (s/set (s/update "foo") x))
        basic (s/update "foo")
        t1 (s/set basic ["bar" "baz"])
        t2 (s/where t1 [:op "and" [[:value true]
                                   [:op "=" [[:identifier ["foo" "bar"]]
                                             [:value false]]]]])]
    (testing "map generation"
      (is (thrown-with-msg? Exception insufficient-data (gen-err [])))
      (is (thrown-with-msg? Exception expected-vector (gen-err {})))
      (is (thrown-with-msg? Exception expected-vector (gen-err {:foo :bar})))
      (let [e1 {:type :update :table "foo"
                :set [[[:identifier ["bar"]] [:placeholder]]
                      [[:identifier ["baz"]] [:placeholder]]]}
            e2 {:type :update :table "foo"
                :set [[[:identifier ["bar"]] [:placeholder]]
                      [[:identifier ["baz"]] [:placeholder]]]
                :where [:op "and" [[:value true]
                                   [:op "="
                                    [[:identifier ["foo" "bar"]]
                                     [:value false]]
                                    {}]]
                                  {}]}]
        (is (= t1 e1))
        (is (= t2 e2))))
  (testing "sql generation"
    (is (thrown-with-msg? Exception expected-vector (s/render (s/update "foo"))))
    (let [s1 (s/render t1)
          e1 "UPDATE foo SET \"bar\" = ?, \"baz\" = ?"
          s2 (s/render t2)
          e2 "UPDATE foo SET \"bar\" = ?, \"baz\" = ? WHERE (true and (\"foo\".\"bar\" = false))"]
      (is (= s1 e1))
      (is (= s2 e2))))))


(deftest delete
  (let [t1 (s/delete "foo")
        t2 (-> (s/delete "foo")
               (s/where [:value true]))
        t3 (-> (s/delete "foo")
               (s/where [:op "and" [[:value true] [:value false]]]))
        t4 (-> (s/delete "foo")
               (s/where [:op "=" [[:value 1] [:identifier ["table" "column"]]]]))
        t5 (-> (s/delete "foo")
               (s/where [:op "::bool" [[:value true]] {:postfix true}]))
        t6 (-> (s/delete "foo")
               (s/where [:op :apply ["bool" [[:value true]]]]))
        t7 (-> (s/delete "foo")
               (s/where [:op "in" [[:identifier ["table" "column"]]
                                   [:value 1]
                                   [:value 2]]]))
        t8 (-> (s/delete "foo")
               (s/where [:op "not in" [[:identifier ["table" "column"]]
                                       [:value 1]
                                       [:value 2]]]))
        t9 (-> (s/delete "foo")
               (s/where [:op "between" [[:identifier ["table" "column"]]
                                        [:value 1]
                                        [:value 2]]]))
        t10 (-> (s/delete "foo")
                (s/where [:op "not between" [[:identifier ["table" "column"]]
                                         [:value 1]
                                         [:value 2]]]))]


    (testing "map generation"
      (let [e1 {:type :delete :table "foo"}
            e2 {:type :delete :table "foo" :where [:value true]}
            e3 {:type :delete :table "foo" :where [:op "and"
                                                   [[:value true]
                                                    [:value false]]
                                                   {}]}
            e4 {:type :delete :table "foo" :where [:op "="
                                                   [[:value 1]
                                                    [:identifier ["table" "column"]]]
                                                   {}]}]
        (is (= t1 e1))
        (is (= t2 e2))
        (is (= t3 e3))
        (is (= t4 e4)))) ;; I think we've probably proven the point by now
    (testing "sql generation"
      (let [s1 (s/render t1)
            s2 (s/render t2)
            s3 (s/render t3)
            s4 (s/render t4)
            s5 (s/render t5)
            s6 (s/render t6)
            s7 (s/render t7)
            s8 (s/render t8)
            s9 (s/render t9)
            s10 (s/render t10)
            e1 "DELETE FROM foo"
            e2 "DELETE FROM foo WHERE true"
            e3 "DELETE FROM foo WHERE (true and false)"
            e4 "DELETE FROM foo WHERE (1 = \"table\".\"column\")"
            e5 "DELETE FROM foo WHERE (true)::bool"
            e6 "DELETE FROM foo WHERE bool(true)"
            e7 "DELETE FROM foo WHERE (\"table\".\"column\" in (1, 2))"
            e8 "DELETE FROM foo WHERE (\"table\".\"column\" not in (1, 2))"
            e9 "DELETE FROM foo WHERE (\"table\".\"column\" between 1 and 2)"
            e10 "DELETE FROM foo WHERE (\"table\".\"column\" not between 1 and 2)"]
            
        (is (= s1 e1))
        (is (= s2 e2))
        (is (= s3 e3))
        (is (= s4 e4))
        (is (= s5 e5))
        (is (= s6 e6))
        (is (= s7 e7))
        (is (= s8 e8))
        (is (= s9 e9))
        (is (= s10 e10))))))
