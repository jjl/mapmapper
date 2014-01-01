(ns mapmapper.sql
  (:require [clojure.string :as string]
            [mapmapper.util :as u]
            [mapmapper.sql.transform :as t])
  (:refer-clojure :except [set]))

(declare -render-expr)

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

(defn -render-identifier [i]
  (str \" (string/join "\".\"" (get i 1)) \"))

(defn -render-placeholder [& ignored]
  "?")

(defn insert [table cols]
  {:type :insert
   :table table
   :cols (t/munge-insert cols)})

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
  (assoc query :set (t/munge-set fields)))

(def where-atom-types
  [:identifier :value :op])

(defn where [query w]
  (assoc query :where (t/munge-where w)))

(defn from [query f]
  (assoc query :from (t/munge-from f)))

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

(defn -render-value [[kw value]]
  (cond
   (u/is-numeric? value) (str value)
   (u/is-boolean? value) (str value)
   (u/is-string? value) (str \' value \')
   :default (throw (Exception. (str "don't know how to handle a " (class value))))))

(defn -render-nonunary-op [[kw op args]]
  (let [mapped (map -render-expr args)]
    (cond (is-forced-binop? op)
          (let [[head & tail] mapped]
            (str "(" head " " op " (" (string/join ", " tail) "))"))
          (is-forced-ternop? op)
          (let [[head lbound ubound] mapped]
            (str "(" head " " op " " lbound " and " ubound ")"))
          :default
          (let [joiner (str " " op " ")]
            (str "(" (string/join joiner mapped) ")")))))

(defn -render-function-application [[kw1 kw2 [func args]]]
  (let [rendered (map -render-expr args)]
    (str func "(" (string/join ", " rendered) ")")))

(defn -render-unary-op [[kw op args & maybe-metadata :as input]]
  (let [metadata (-maybe-metadata maybe-metadata)
        postfix (:postfix metadata)]
    (if postfix
      (str "(" (-render-expr (first args)) ")" op)
      (str op " " (-render-expr args)))))

(defn -render-unknown-op [[kw op args & maybe-metadata :as input]]
  ((if (= (count args) 1)
    -render-unary-op
    -render-nonunary-op) input))
  
(defn -render-op [[kw op args & maybe-metadata :as input]]
  ((if (or (= op :apply) (= op "apply"))
     -render-function-application
     -render-unknown-op) input))

(defn -render-where [query]
  (let [where (get query :where)]
    (when where
      (str "WHERE " (-render-expr where)))))

(defn -maybe-where [base query]
  (let [where (-render-where query)]
    (if where
      (str base " " where)
      base)))

(defn -render-raw [[kw arg]]
  (str arg))

(defn -render-expr [[head & tail :as expr]]
  (cond
   (= head :raw) (-render-raw expr)
   (= head :identifier) (-render-identifier expr)
   (= head :value) (-render-value expr)
   (= head :op) (-render-op expr)
   (= head :placeholder) (-render-placeholder)
   :default (u/unexpected-err expr)))

(defn -render-set-expr [[l r]]
  (let [left (-render-identifier l)
        right (-render-expr r)]
    (str left " = " right)))
        
(defn -render-set [query]
  (let [fields (get query :set [])
        pieces (map -render-set-expr fields)]
    (str "SET " (string/join ", " pieces))))

(defn -render-insert [query]
  (let [cols (:cols query)
        table (:table query)
        q-cols (string/join ", "
                            (map (comp -render-identifier
                                       first) cols))
;; TODO: we should check what to render here, not assume placeholders
        q-vals (string/join ", "
                            (map (comp -render-expr
                                       second) cols))]
    (string/join " " ["INSERT INTO"
                      table "(" q-cols ")"
                      "VALUES" "(" q-vals ")"])))

(defn -render-select [query]
  "not implemented")

(defn -render-update [query]
  (let [where (get query :where {})
        table (:table query)
        base-query (string/join " " ["UPDATE" table])
        set (t/munge-set (:set query))
        set-part (-render-set (assoc query :set set))
        base-set (string/join " " [base-query set-part])]
    (-maybe-where base-set query)))

(defn -render-delete [query]
  (let [where (get query :where {})
        table (:table query)
        base-query (string/join " " ["DELETE FROM" table])]
    (-maybe-where base-query query)))

(defn generate [query]
  (let [type (:type query)]
    (cond (= type :insert) (-render-insert query)
          (= type :select) (-render-select query)
          (= type :update) (-render-update query)
          (= type :delete) (-render-delete query))))

