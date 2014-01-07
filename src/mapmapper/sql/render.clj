(ns mapmapper.sql.render
  (:require [mapmapper.util :as u]
            [mapmapper.sql.transform :as t]
            [clojure.string :as string]))

(declare -render-expr render)

(def -default-forced-binops
  ["in" "not in"])
(def -default-forced-ternops
  ["between" "not between"])

(defn is-forced-ternop? [thing]
  (not= (.indexOf -default-forced-ternops thing) -1))
(defn is-forced-binop? [thing]
  (not= (.indexOf -default-forced-binops thing) -1))

(defn -maybe-metadata [item]
  (when (and (seq item)
             (= (count item) 1))
    (let [metadata (first item)]
      (if (map? metadata)
        metadata
        {}))))

(defn -render-identifier [i]
  (str \" (string/join "\".\"" (get i 1)) \"))

(defn -render-placeholder [& ignored]
  "?")

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

(defn -render-function-application [[kw1 kw2 [func args] :as input]]
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
  (if (or (= op :apply) (= op "apply"))
    (-render-function-application input)
    (-render-unknown-op input)))

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

(defn render-insert [query]
  (let [fields (:fields query)
        table (:table query)
        q-fields (string/join ", "
                              (map (comp -render-identifier
                                         first) fields))
        q-vals (string/join ", "
                            (map (comp -render-expr
                                       second) fields))]
    (string/join " " ["INSERT INTO"
                      table "(" q-fields ")"
                      "VALUES" "(" q-vals ")"])))

(defn -render-select-fields [f]
  (string/join ", "
               (map (fn [[type & rest :as input]]
                      (condp = type
                        :identifier (-render-identifier input)
                        :raw (-render-raw input)
                        (u/unexpected-err f))) f)))

(defn -render-table [[kw table]]
  (str \" table \"))

(defn -render-subquery [[kw q]]
  (str "(" (render q) ")"))

(defn -render-alias [[kw [atype & arest :as arg] alias :as input]]
  (let [next (condp = atype
               :table (-render-table arg)
               :raw (-render-raw arg)
               :query (-render-subquery arg)
               (u/unexpected-err arg))]
    (string/join " " [next "AS" alias])))

(defn -render-join [[kw [t1type & t1args :as t1] [t2type & t2args :as t2] {:keys [type on] :as meta} :as input]]
  (let [r-t1 (condp = t1type
               :table (-render-table t1)
               :join (throw (Exception. "The first argument to a join can't be another join. They flow RIGHTwards"))
               :alias (-render-alias t1)
               (u/unexpected-err t1))
        r-t2 (condp = t2type
               :table (-render-table t2)
               :join (-render-join t2)
               :alias (-render-alias t2)
               (u/unexpected-err t2))
        join-phrase (condp type =
                 :left "LEFT JOIN"
                 :right "RIGHT JOIN"
                 :inner "INNER JOIN"
                 :cross "CROSS JOIN")
        on-phrase (when (seq on)
                    (-render-op on))]
    (if (= type :cross)
      (string/join " " [r-t1 join-phrase r-t2])
      (string/join " " [r-t1 join-phrase r-t2 "ON" on-phrase]))))

    
(defn -render-select-from [f]
  (string/join ", "
               (map (fn [[type & rest :as input]]
                      (condp = type
                        :table (-render-table input)
                        :join (-render-join input)
                        :alias (-render-alias input)
                        ;; We don't support these yet
                        ;; :lateral (u/unexpected-err input)
                        ;; :tablefunc (u/unexpected-err input)
                        (u/unexpected-err input))) f)))

(defn render-select [query]
  (let [{:keys [fields from where]} query
        s-fields (-render-select-fields fields)
        s-from (-render-select-from from)
        base-query (string/join " " ["SELECT" s-fields
                                     "FROM" s-from])]
    (-maybe-where base-query query)))

(defn render-update [query]
  (let [{:keys [where table]} query
        base-query (string/join " " ["UPDATE" table])
        set (t/munge-set (:set query))
        set-part (-render-set (assoc query :set set))
        base-set (string/join " " [base-query set-part])]
    (-maybe-where base-set query)))

(defn render-delete [query]
  (let [{:keys [where table]} query
        base-query (string/join " " ["DELETE FROM" table])]
    (-maybe-where base-query query)))

(defn render [q]
  (let [type (:type q)]
    (condp = type
      :select (render-select q)
      :update (render-update q)
      :insert (render-insert q)
      :delete (render-delete q)
      (u/unexpected-err q))))
  
