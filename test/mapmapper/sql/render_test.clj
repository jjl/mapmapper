(ns mapmapper.sql.render_test
  (:require [midje.sweet :refer :all]
            [mapmapper.sql.transform :as t]
            [mapmapper.sql :as s]
            [mapmapper.sql.render :as r]))

(facts "rendering"
       (fact "select"
             (let [t1 {:type :select
                       :fields [[:identifier ["bar"]] [:identifier ["baz"]]]
                       :from [[:table "foo"]]
                       :where [:value true]}
                   t2 {:type :select
                       :distinct true
                       :on [[:identifier ["bar" "b1"]] [:identifier ["foo" "f1"]]]
                       :fields [[:identifier ["foo" "f3"]] [:identifier ["bar" "b3"]]]
                       :from [[:join [:table "foo"] [:table "bar"] {:type :left :on [:op "=" [[:identifier ["foo" "f2"]]
                                                                                              [:identifier ["bar" "b2"]]]]}]]}]
               (r/render t1) => "SELECT \"bar\", \"baz\" FROM \"foo\" WHERE true"
               (r/render t2) => "SELECT DISTINCT ON ( \"bar\".\"b1\", \"foo\".\"f1\" ) \"foo\".\"f3\", \"bar\".\"b3\" FROM \"foo\" LEFT JOIN \"bar\" ON (\"foo\".\"f2\" = \"bar\".\"b2\")")))
             
