(ns mapmapper.sql
  (:require [clojure.string :as string]
            [mapmapper.util :as u])
  (:refer-clojure :except [set]))

(declare -where-expr -where-op -validate-where-expr -validate-set -validate-identifier)

(def -default-forced-binops
  ["in" "not in"])
(def -default-forced-ternops
  ["between" "not between"])

(defn is-forced-ternop? [thing]
  (not= (.indexOf -default-forced-ternops thing) -1))
(defn is-forced-binop? [thing]
  (not= (.indexOf -default-forced-binops thing) -1))

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

(defn set [query fields]
  (-validate-set fields)
  (assoc query :set fields))

(def where-atom-types
  [:identifier :value :op])

(defn -validate-set [fields]
  (when-not (and (seq fields)
                 (not (map? fields)))
    (throw (Exception. "Expected fields to set")))
  (every? -validate-identifier fields))

(defn -valid-where-atom [[head & tail]]
  (and (keyword? head)
       (not= -1 (.indexOf where-atom-types head))))

(defn -valid-value [item]
  (and (seq item)
       (not (map? item))
       (= (count item) 1)))

(defn -valid-binop-data [item]
  (and (seq item)
       (not (map? item))
       (= (count item) 2)))
  
(defn -valid-ternop-data [item]
  (and (seq item)
       (not (map? item))
       (= (count item) 3)))

(defn -validate-value-list [val-list]
  (if (-valid-value val-list)
    true
    (throw (Exception. (str "invalid value: " val-list)))))

(defn -validate-boolean-value [val-list]
  (-validate-value-list val-list)
  (if (u/is-boolean? (first val-list))
    true
    (throw (Exception. (str "Expected boolean value: " val-list)))))

(defn -validate-where-op [[op args]]
  (cond
   (or (= op :apply)
       (= op "apply"))
   (do 
     (when-not (vector? args)
       (throw (Exception. "Function application requires a vector of arguments")))
     (when-not (> (count args) 0)
       (throw (Exception. (str "Expected function name " args))))
     (when (> (count args) 0)
            (map -validate-where-expr args))
     (when (and (is-forced-binop? op)
                (not= (count args) 2))
       (throw (Exception. (str "Found binop " op " but found " (count args) " arguments"))))
     (when (and (is-forced-ternop? op)
                (not= (count args) 3))
       (throw (Exception. (str "Found ternop " op " but found " (count args) " arguments")))))
   (u/is-string? op)
   (map -validate-where-expr args)))

(defn -validate-identifier [i]
  (when-not (u/is-string? i)
    (throw (Exception. "Expected string for identifier"))))

(defn -validate-identifier-list [l]
  (when-not (vector? l)
    (throw (Exception. "Expected vector for identifier list")))
  (when-not (= (count l) 1)
    (throw (Exception. "Expected single vector for identifier")))
  (map (comp -validate-identifier first) l))

(defn -validate-where-expr [w]
  (let [[type & args] w]
    (condp = type
      :identifier (-validate-identifier-list args)
      :value (-validate-value-list args)
      :op (-validate-where-op args)
      (throw (Exception. (str "Invalid where atom: " w))))))
  
(defn -validate-where [w]
  (let [[type & args] w]
    (condp = type
      :identifier (throw (Exception. "unexpected identifier at toplevel"))
      :value (-validate-boolean-value args)
      :op (-validate-where-op args)
      (throw (Exception. (str "Invalid where atom: " w))))))

(defn -validate-from [f]
  true)
  
(defn where [query w]
  (-validate-where w)
  (assoc query :where w))

(defn from [query f]
  (-validate-from f)
  (assoc query :from f))

 ;; [:identifier ["name" "pieces"]]
 ;; [:value val]
 ;; [:op name [args] {meta data}]

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
    :value (let [val (first args)]
             (-stringify-value val))
    :op (-where-op args)
    (throw (Exception. (str "-where-expr: " type)))))

(defn -where-bin-or-multi-op [op args]
  (let [mapped (map -where-expr args)]
    (cond (is-forced-binop? op)
          (let [[head & tail] mapped]
            (str "(" head " " op " (" (string/join ", " tail) "))"))
          (is-forced-ternop? op)
          (let [[head lbound ubound] mapped]
            (str "(" head " " op " " lbound " and " ubound ")"))
          :default
          (let [joiner (str " " op " ")]
            (str "(" (string/join joiner mapped) ")")))))

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
                 (-where-bin-or-multi-op op args)))))

(defn -generate-where [query]
  (let [where (get query :where)]
    (when where
       (let [[type & args] where]
          (condp = type
            :value (let [val (first args)]
                     (str "WHERE " val))
            :op (str "WHERE " (-where-op args)))))))

(defn -maybe-where [base query]
  (let [where (-generate-where query)]
    (if where
      (str base " " where)
      base)))

(defn -generate-set [query]
  (let [fields (get query :set [])
        table (:table query)
        qualified-fields (map #(-quote-identifier [table %]) fields)]
    (-validate-set fields)
    (when (seq fields)
      (str "SET "
           (string/join ", "
                        (map #(str % " = ?") qualified-fields))))))

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
  (let [where (get query :where {})
        table (:table query)
        base-query (string/join " " ["UPDATE" table])
        set-part (-generate-set query)
        base-set (string/join " " [base-query set-part])]
    (-maybe-where base-set query)))

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

