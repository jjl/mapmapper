(ns mapmapper.sql.transform_test
  (:require [clojure.test :refer :all]
            [mapmapper.sql.transform :as t]))

(deftest utilities
  (testing "veclike?"
    (let [trues [[] '() (seq [1 2 3])]
          falses [{} 1 "foo"]]
      (is (every? true? (map t/veclike? trues)))
      (is (every? (comp not true?) (map t/veclike? falses)))))
  (testing "-munge-identifier"
    (let [mapping [["foo" [:identifier ["foo"]]]
                   [["foo" "bar"] [:identifier ["foo" "bar"]]]
                   [[:identifier "foo"] [:identifier ["foo"]]]
                   [[:identifier ["foo"]] [:identifier ["foo"]]]
                   [[:identifier "foo" "bar"] [:identifier ["foo" "bar"]]]]
          unex #"^Unexpected data:"
          -mi t/-munge-identifier]
      (is (every? (fn [[l r]]
                    (= (-mi l) r)) mapping))
      (is (thrown-with-msg? Exception unex (-mi [:identifier])))
      (is (thrown-with-msg? Exception unex (-mi [:identifier 1])))
      (is (thrown-with-msg? Exception unex (-mi [:identifier [1]])))
      (is (thrown-with-msg? Exception unex (-mi [:identifier "foo" 1])))
      (is (thrown-with-msg? Exception unex (-mi [:identifier {}])))
      (is (thrown-with-msg? Exception unex (-mi [1])))
      (is (thrown-with-msg? Exception unex (-mi [{}])))
      (is (thrown-with-msg? Exception unex (-mi [])))))
  (testing "-munge-value"
    (let [invalid #"^Expected: :value, got:"
          len #"^Expected collection of length:"
          vec #"^Expected vector"
          -mv t/-munge-value]
      (is (thrown-with-msg? Exception len (-mv [])))
      (is (thrown-with-msg? Exception len (-mv [:value])))
      (is (thrown-with-msg? Exception invalid (-mv [:foo :bar])))
      (is (thrown-with-msg? Exception vec (-mv {})))))
  (testing "-munge-raw"
    (let [len #"^Expected collection of length:"
          vec #"^Expected vector"
          raw #"^Expected: :raw, got: "
          string #"^Expected string:"
          -mr t/-munge-raw]

      (is (thrown-with-msg? Exception vec (-mr {})))
      (is (thrown-with-msg? Exception len (-mr [])))
      (is (thrown-with-msg? Exception len (-mr [:foo])))
      (is (thrown-with-msg? Exception string (-mr [:foo :bar])))
      (is (thrown-with-msg? Exception raw (-mr [:foo "bar"])))
      (is (thrown-with-msg? Exception string (-mr [:raw 1])))))
  (testing "-munge-placeholder"
    (let [inv #"^Expected: :placeholder, got:"
          vec #"^Expected vector"
          len #"^Expected collection of length:"
          -mp t/-munge-placeholder]
      (is (thrown-with-msg? Exception vec (-mp {})))
      (is (thrown-with-msg? Exception len (-mp [])))
      (is (thrown-with-msg? Exception len (-mp [:placeholder :foo])))
      (is (thrown-with-msg? Exception inv (-mp [:foo])))))
  (testing "-munge-expr"
    (let [vec #"^Expected vector"
          len #"^Expected collection of minimum length:"
          unex #"^Unexpected data:"
          -me t/-munge-expr]
      (is (thrown-with-msg? Exception vec (-me {})))
      (is (thrown-with-msg? Exception len (-me [])))
      (is (thrown-with-msg? Exception unex (-me [:foo])))))
  (testing "-munge-set-atom"
    ;; It would be nice to tighten these up
    (let [unex #"^Unexpected data:"
          -msa t/-munge-set-atom
          -mi t/-munge-identifier]
      (is (thrown-with-msg? Exception unex (-msa {})))
      (is (= (-mi "foo")
             (first (-msa [:identifier "foo"]))))
      (is (= (-mi "foo")
             (first (-msa [["foo"] [:value 1]]))))
      (is (= (-mi "foo")
             (first (-msa "foo"))))))
  (testing "munge-set"
    (let [vec #"^Expected vector"
          len #"^Expected collection of minimum length:"
          ms t/munge-set]
      (is (thrown-with-msg? Exception vec (ms {})))
      (is (thrown-with-msg? Exception len (ms []))))))
                       
              
