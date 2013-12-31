(ns mapmapper.sql
  (:require [clojure.string :as string]
            [mapmapper.util :as u]))

(declare -where-expr -where-op)

(defn -alias-table [table aliases]
  (let [next (str "t" (-> aliases keys count))
        new-aliases (assoc aliases next table)]
    [next new-aliases]))

(defn -quote-identifier [pieces]
  (str \" (string/join "\".\"" pieces) \"))

(defn -generate-placeholders [count]
  (let [prefix (repeat (dec count) "?,")
        suffix (when (> count 0) ["?"])
        combined (concat prefix suffix)]
    (string/join " " combined)))

(defn -qualified-names [table cols]
  (string/join ", " (map #(-quote-identifier [table %]) cols)))

(defn insert [table cols]
  {:type :insert
   :table table
   :cols cols})

(defn delete [table]
  {:type :delete
   :table table})

(defn select [table]
  {:type :select
   :table table})

(defn update [table]
  {:type :update
   :table table})

(def where-atom-types
  [:identifier :value :op])
  
(defn where [query w]
  (assoc query :where w))

 ;; [:identifier ["name" "pieces"]]
 ;; [:value val]
 ;; [:op name [args] {meta data}]

(defn -valid-where-atom [[head & tail]]
  (and (keyword? head)
       (not= -1 (.indexOf where-atom-types head))))

(defn -valid-value [item]
  (and (seq item)
       (not (map? item))
       (= (count item) 1)))

(defn -maybe-metadata [item]
  (when (and (seq item)
             (= (count item) 1))
    (let [metadata (first item)]
      (if (map? metadata)
        metadata
        {}))))

(defn -stringify-value [value]
  (cond
   (u/is-numeric? value) (str value)
   (u/is-boolean? value) (str value)
   (u/is-string? value) (str \' value \')
   :default (throw (Exception. (str "don't know how to handle a " (class value))))))

(defn -where-expr [[type & args]]
  (condp = type
    :identifier (-quote-identifier (first args))
    :value (if (-valid-value args)
             (let [val (first args)]
               (-stringify-value val))
             (throw (Exception. (str "expected value: " val))))
    :op (-where-op args)
    (throw (Exception. (str "-where-expr: " type)))))

(defn -where-op [[op args & maybe-metadata]]
  (let [[func fargs] args
        metadata (-maybe-metadata maybe-metadata)]
    (cond
     (or (= op :apply)
         (= op "apply")) (let [mapped (map -where-expr fargs)]
                           (str func "(" (string/join ", " mapped) ")"))
         :else (condp = (count args)
           1 (let [postfix (get metadata :postfix false)]
               (if postfix
                 (str "(" (-where-expr (first args)) ") " op)
                 (str op " " (-where-expr args))))
           (let [mapped (map -where-expr args)
                 joiner (str " " op " ")]
             (str "(" (string/join joiner mapped) ")"))))))

(defn -generate-where [query]
  (let [where (get query :where)]
    (when where
       (if-not (-valid-where-atom where)
         (throw (IllegalArgumentException. "Invalid where atom")))
       (let [[type & args] where]
          (condp = type
            :identifier (throw (Exception. "Toplevel: unexpected identifier"))
            :value (let [val (first args)]
                     (if (instance? java.lang.Boolean val)
                       (str "WHERE " val)
                       (throw (Exception. "Toplevel: unexpected non-boolean value"))))
            :op (str "WHERE " (-where-op args)))))))

(defn -maybe-where [base query]
  (let [where (-generate-where query)]
    (if where
      (str base " " where)
      base)))

(defn -generate-insert [query]
  (let [cols (:cols query)
        table (:table query)
        q-cols (-qualified-names table cols)
        placeholders (-generate-placeholders (count cols))]
    (string/join " " ["INSERT INTO"
                      table "(" q-cols ")"
                      "VALUES" "(" placeholders ")"])))

(defn -generate-select [query]
  "not implemented")
(defn -generate-update [query]
  "not implemented")
(defn -generate-delete [query]
  (let [where (get query :where {})
        table (:table query)
        base-query (string/join " " ["DELETE FROM" table])]
    (-maybe-where base-query query)))

(defn generate [query]
  (let [type (:type query)]
    (cond (= type :insert) (-generate-insert query)
          (= type :select) (-generate-select query)
          (= type :update) (-generate-update query)
          (= type :delete) (-generate-delete query))))

