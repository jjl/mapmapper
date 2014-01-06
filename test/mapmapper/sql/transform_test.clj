(ns mapmapper.sql.transform_test
  (:require [midje.sweet :refer :all]
            [mapmapper.sql.transform :as t]))

(facts "utilities"
       (fact "veclike?"
             (let [trues [[] '() (seq [1 2 3])]
                   falses [{} 1 "foo"]]
               (map t/veclike? trues) => (has every? true?)
               (map t/veclike? falses) => (has not-any? true?)))
       (fact "-mandate-vector")
       (fact "-mandate-length")
       (fact "-mandate-min-length")
       (fact "-mandate-string")
       (fact "-mandate-first"))
(facts "munging"
       (fact "-munge-identifier"
             (let [mapping [["foo" [:identifier ["foo"]]]
                            [["foo" "bar"] [:identifier ["foo" "bar"]]]
                            [[:identifier "foo"] [:identifier ["foo"]]]
                            [[:identifier ["foo"]] [:identifier ["foo"]]]
                            [[:identifier "foo" "bar"] [:identifier ["foo" "bar"]]]]
                   unex #"^Unexpected data:"
                   -mi t/-munge-identifier]
               (map #(-mi (first %)) mapping) => (map second mapping)
               (-mi [:identifier])            => (throws Exception unex)
               (-mi [:identifier 1])          => (throws Exception unex)
               (-mi [:identifier [1]])        => (throws Exception unex)
               (-mi [:identifier "foo" 1])    => (throws Exception unex)
               (-mi [:identifier {}])         => (throws Exception unex)
               (-mi [1])                      => (throws Exception unex)
               (-mi [{}])                     => (throws Exception unex)
               (-mi [])                       => (throws Exception unex)))
       (fact "-munge-value"
             (let [invalid #"^Expected: :value, got:"
                   len #"^Expected collection of length:"
                   vec #"^Expected vector"
                   -mv t/-munge-value]
               (-mv [])          => (throws Exception len)
               (-mv [:value])    => (throws Exception len)
               (-mv [:foo :bar]) => (throws Exception invalid)
               (-mv {}) => (throws Exception vec)))
       (fact "-munge-raw"
             (let [len #"^Expected collection of length:"
                   vec #"^Expected vector"
                   raw #"^Expected: :raw, got: "
                   string #"^Expected string:"
                   -mr t/-munge-raw]
               (-mr {})           => (throws Exception vec)
               (-mr [])           => (throws Exception len)
               (-mr [:foo])       => (throws Exception len)
               (-mr [:foo :bar])  => (throws Exception string)
               (-mr [:foo "bar"]) => (throws Exception raw)
               (-mr [:raw 1])     => (throws Exception string)))
       (fact "-munge-placeholder"
             (let [inv #"^Expected: :placeholder, got:"
                   vec #"^Expected vector"
                   len #"^Expected collection of length:"
                   -mp t/-munge-placeholder]
               (-mp {})                  => (throws Exception vec)
               (-mp [])                  => (throws Exception len)
               (-mp [:placeholder :foo]) => (throws Exception len)
               (-mp [:foo])              => (throws Exception inv)))
       (fact "-munge-expr"
             (let [vec #"^Expected vector"
                   len #"^Expected collection of minimum length:"
                   unex #"^Unexpected data:"
                   -me t/-munge-expr]
               (-me {})     => (throws Exception vec)
               (-me [])     => (throws Exception len)
               (-me [:foo]) => (throws Exception unex)))
       (fact "-munge-set-atom"
             ;; It would be nice to tighten these up
             (let [unex #"^Unexpected data:"
                   -msa t/-munge-set-atom
                   -mi t/-munge-identifier]
               (-msa {})                           => (throws Exception unex)
               (first (-msa [:identifier "foo"]))  => (-mi "foo")
               (first (-msa [["foo"] [:value 1]])) => (-mi "foo")
               (first (-msa "foo"))                => (-mi "foo")))
       (fact "munge-set"
             (let [vec #"^Expected vector"
                   len #"^Expected collection of minimum length:"
                   ms t/munge-set]
               (ms {}) => (throws Exception vec)
               (ms []) => (throws Exception len)))
       (fact "-munge-alias"
             (let [string #"^Expected string:"
                   unex #"^Unexpected data:"
                   tf "Don't support table function FROM sources yet"
                   -ma t/-munge-alias]
               (-ma [:alias [:foo] :bar] :from) => (throws Exception string)
               (-ma [:alias [:tablefunc] "foo"] :from) => (throws Exception tf)))
       (fact "-munge-join-meta")
       (fact "-munge-join")
       (fact "-munge-lateral")
       (fact "-munge-table")
       (fact "-munge-op")
       (fact "-munge-group-by")
       (fact "-munge-having")
       (fact "-munge-window")
       (fact "-munge-limit")
       (fact "-munge-offset")
       (fact "-munge-fetch")
       (fact "-munge-for")
       (fact "-munge-with")
       (fact "munge-where")
       (fact "munge-insert")
       (fact "munge-fields")
       ;; These could get rather long and tedious...
       (fact "munge-from")
       (fact "-munge-query"))
        
