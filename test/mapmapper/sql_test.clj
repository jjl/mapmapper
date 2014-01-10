(ns mapmapper.sql_test
  (:require [midje.sweet :refer :all]
            [mapmapper.sql :as s]))

(facts "maps"
    (fact "insert"
          (let [t1 (-> (s/insert "foo")
                       (s/fields ["bar" "baz" "quux"]))]
            t1 => {:type :insert :table "foo"
                   :fields [[[:identifier ["bar"]] [:placeholder]]
                            [[:identifier ["baz"]] [:placeholder]]
                            [[:identifier ["quux"]] [:placeholder]]]}))
    (fact "select"
          (let [t1 (-> (s/select "foo")
                       (s/from [[:table "foo"]])
                       (s/fields ["bar" "baz" "quux"]))]
            (let [e1 {:type :select :from [[:table "foo"]] :fields [[:identifier ["bar"]]
                                                                    [:identifier ["baz"]]
                                                                    [:identifier ["quux"]]]
                      :meta {}}]
              t1 => e1)))
    (fact "update"
          (let [insufficient-data #"^Expected collection of minimum length:"
                expected-vector #"^Expected vector:"
                gen-err (fn [x] (s/set (s/update "foo") x))
                basic (s/update "foo")
                t1 (s/set basic ["bar" "baz"])
                t2 (s/where t1 [:op "and" [[:value true]
                                           [:op "=" [[:identifier ["foo" "bar"]]
                                                     [:value false]]]]])]
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
              t2 => e2)))
    (fact "delete"
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
            t1 => {:type :delete :table "foo"}
            t2 => {:table "foo", :type :delete, :where [:value true]}
            t3 => {:type :delete :table "foo" :where [:op "and" [[:value true]
                                                                 [:value false]]
                                                      {}]}
            t4 => {:type :delete :table "foo" :where [:op "="
                                                      [[:value 1]
                                                       [:identifier ["table" "column"]]]
                                                      {}]})))
