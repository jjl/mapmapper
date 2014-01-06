(ns mapmapper.sql_test
  (:require [midje.sweet :refer :all]
            [mapmapper.sql :as s]))

(facts "insert"
  (let [t1 (s/fields (s/insert "foo") ["bar" "baz" "quux"])]
    (fact "map generation"
          t1 => {:type :insert :table "foo"
                 :fields [[[:identifier ["bar"]] [:placeholder]]
                          [[:identifier ["baz"]] [:placeholder]]
                          [[:identifier ["quux"]] [:placeholder]]]})
    (fact "sql generation"
          (s/render t1) => "INSERT INTO foo ( \"bar\", \"baz\", \"quux\" ) VALUES ( ?, ?, ? )")))

(facts "select"
  (let [t1 (-> (s/select "foo")
               (s/from [[:table "foo"]])
               (s/fields ["bar" "baz" "quux"]))]
    (fact "map generation"
      (let [e1 {:type :select :from [[:table "foo"]] :fields [[:identifier ["bar"]]
                                                              [:identifier ["baz"]]
                                                              [:identifier ["quux"]]]
                :meta {}}]
        t1 => e1))
    (fact "sql generation"
        (s/render t1) => "SELECT \"bar\", \"baz\", \"quux\" FROM \"foo\"")))

(facts "update"
  (let [insufficient-data #"^Expected collection of minimum length:"
        expected-vector #"^Expected vector:"
        gen-err (fn [x] (s/set (s/update "foo") x))
        basic (s/update "foo")
        t1 (s/set basic ["bar" "baz"])
        t2 (s/where t1 [:op "and" [[:value true]
                                   [:op "=" [[:identifier ["foo" "bar"]]
                                             [:value false]]]]])]
    (fact "map generation"
      (gen-err [])          => (throws Exception insufficient-data)
      (gen-err {})          => (throws Exception expected-vector)
      (gen-err {:foo :bar}) => (throws Exception expected-vector)
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
        t1 => e1
        t2 => e2))
  (fact "sql generation"
    (-> "foo" s/update s/render) => (throws Exception expected-vector)
      (s/render t1) => "UPDATE foo SET \"bar\" = ?, \"baz\" = ?"
      (s/render t2) => "UPDATE foo SET \"bar\" = ?, \"baz\" = ? WHERE (true and (\"foo\".\"bar\" = false))")))



(facts "delete"
  (let [t1 (s/delete "foo")
        t2 (-> t1 (s/where [:value true]))
        t3 (-> t1 (s/where [:op "and" [[:value true] [:value false]]]))
        t4 (-> t1 (s/where [:op "=" [[:value 1] [:identifier ["table" "column"]]]]))
        t5 (-> t1 (s/where [:op "::bool" [[:value true]] {:postfix true}]))
        t6 (-> t1 (s/where [:op :apply ["bool" [[:value true]]]]))
        t7 (-> t1 (s/where [:op "in" [[:identifier ["table" "column"]]
                                      [:value 1]
                                      [:value 2]]]))
        t8 (-> t1 (s/where [:op "not in" [[:identifier ["table" "column"]]
                                          [:value 1]
                                          [:value 2]]]))
        t9 (-> t1 (s/where [:op "between" [[:identifier ["table" "column"]]
                                           [:value 1]
                                           [:value 2]]]))
        t10 (-> t1 (s/where [:op "not between" [[:identifier ["table" "column"]]
                                                [:value 1]
                                                [:value 2]]]))]
    (fact "map generation"
         t1 => {:type :delete :table "foo"}
         t2 => {:table "foo", :type :delete, :where [:value true]}
         t3 => {:type :delete :table "foo" :where [:op "and" [[:value true]
                                                              [:value false]]
                                                   {}]}
         t4 => {:type :delete :table "foo" :where [:op "="
                                                   [[:value 1]
                                                    [:identifier ["table" "column"]]]
                                                   {}]})
    (fact "sql generation"
          (s/render t1) => "DELETE FROM foo"
          (s/render t2) => "DELETE FROM foo WHERE true"
          (s/render t3) => "DELETE FROM foo WHERE (true and false)"
          (s/render t4) => "DELETE FROM foo WHERE (1 = \"table\".\"column\")"
          (s/render t5) => "DELETE FROM foo WHERE (true)::bool"
          (s/render t6) => "DELETE FROM foo WHERE bool(true)"
          (s/render t7) => "DELETE FROM foo WHERE (\"table\".\"column\" in (1, 2))"
          (s/render t8) => "DELETE FROM foo WHERE (\"table\".\"column\" not in (1, 2))"
          (s/render t9) => "DELETE FROM foo WHERE (\"table\".\"column\" between 1 and 2)"
          (s/render t10) => "DELETE FROM foo WHERE (\"table\".\"column\" not between 1 and 2)")))
