(ns mapmapper.sql.transform_test
  (:require [midje.sweet :refer :all]
            [mapmapper.sql.transform :as t]))

(facts "utilities"
       (fact "veclike?"
             (let [trues [[] '() (seq [1 2 3])]
                   falses [{} 1 "foo"]]
               (map t/veclike? trues) => (has every? true?)
               (map t/veclike? falses) => (has not-any? true?)))
       (fact "-mandate-vector"
             (let [-mv t/-mandate-vector
                   exp #"Expected vector:"]
               (-mv {})          => (throws Exception exp)
               (-mv #{})         => (throws Exception exp)
               (-mv :a)          => (throws Exception exp)
               (-mv 1)           => (throws Exception exp)
               (-mv "a")         => (throws Exception exp)
               (-mv [])          => nil?
               (-mv '())         => nil?
               (-mv (seq [1 2])) => nil?))
       (fact "-mandate-length"
             (let [-ml t/-mandate-length
                   exp #"^Expected collection of length:"]
               (-ml [] 0)    => nil?
               (-ml [1] 1)   => nil?
               (-ml [1 2] 2) => nil?
               (-ml [] 1)    => (throws Exception exp)
               (-ml [1 2] 1) => (throws Exception exp)
               (-ml [1 2] 3) => (throws Exception exp)))
       (fact "-mandate-min-length"
             (let [-mml t/-mandate-min-length
                   exp #"^Expected collection of minimum length:"]
               (-mml [] 0)  => nil?
               (-mml [1] 0) => nil?
               (-mml [] 1)  => (throws Exception exp)))
       (fact "-mandate-string"
             (let [-ms t/-mandate-string
                   exp #"^Expected string"]
               (-ms "")  => nil?
               (-ms [])  => (throws Exception exp)
               (-ms 1)   => (throws Exception exp)
               (-ms '()) => (throws Exception exp)))
       (fact "-mandate-first"
             (let [-mf t/-mandate-first
                   exp #"^Expected:"]
               (-mf [:a]  :a)  => nil?
               (-mf [1]   1)   => nil?
               (-mf ["a"] "a") => nil?
               (-mf [] :a)     => (throws Exception exp)
               (-mf ["a"] :a)  => (throws Exception exp)
               (-mf [:a] :b)   => (throws Exception exp)
               (-mf [1] 2)     => (throws Exception exp))))
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
               (-me 1)                                 => [:value 1]
               (-me {})                                => (throws Exception vec)
               (-me [])                                => (throws Exception len)
               (-me [:foo])                            => (throws Exception unex)
               (-me [:value "foo"])                    => [:value "foo"]
               (-me [:placeholder])                    => [:placeholder]
               (-me [:identifier "foo"])               => [:identifier ["foo"]]
               (-me [:op "=" [[:value 1] [:value 2]]]) => [:op "=" [[:value 1] [:value 2]] {}]
               (-me [:raw "foo"])                      => [:raw "foo"]))
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
               (-ma [:alias [:foo] :bar] :from)           => (throws Exception string)
               (-ma [:alias [:tablefunc] "foo"] :from)    => (throws Exception tf)
               (-ma [:alias [:join] "foo"] :from)         => (throws Exception unex)
               (-ma [:alias [:table "foo"] "foo"] :from)  => [:alias [:table "foo"] "foo"]
               (-ma [:alias [:raw "foo"] "foo"] :from)    => [:alias [:raw "foo"] "foo"]
               (-ma [:alias [:query {:type :select}] "foo"] :from)  => [:alias [:query {:type :select}] "foo"]
               (-ma [:alias [:query {:type :select}] "foo"] :query) => [:alias [:query {:type :select}] "foo"]
               (-ma [:alias [:raw "foo"] "foo"] :query)   => (throws Exception unex)
               (-ma [:alias [:table "foo"] "foo"] :query) => (throws Exception unex)))
       (fact "-munge-join-meta"
             (let [-mjm t/-munge-join-meta
                   cross "Cross joins cannot have conditions"
                   cond #"joins require a condition"
                   pres "Presumed you wanted an inner join since you didn't specify, but that requires a condition"]
               (-mjm {:type :cross :on [1]}) => (throws Exception cross)
               (-mjm {:type :left}) => (throws Exception cond)
               (-mjm {}) => (throws Exception pres)
               (-mjm {:type :cross}) => {:type :cross}
               (-mjm {:type :left :on [:value true]}) => {:type :left :on [:value true]}))
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
        
