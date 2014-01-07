(ns mapmapper.sql.transform
  (require [mapmapper.util :as u]))

(declare -munge-set-atom munge-from -munge-expr -munge-alias -munge-join -munge-query -munge-identifier -munge-raw)

(defn veclike? [v]
  (or (vector? v)
      (list? v)
      (seq? v)))

(defn -make-upred-mandater [pred name]
  #(when-not (pred %)
     (throw (Exception. (str "Expected " name ": " %)))))

(def -mandate-vector (-make-upred-mandater veclike?      "vector"))
(def -mandate-map    (-make-upred-mandater map?          "map"))
(def -mandate-string (-make-upred-mandater u/is-string?  "string"))
(def -mandate-bool   (-make-upred-mandater u/is-boolean? "boolean"))

(defn -mandate-length [v c]
  (when-not (= (count v) c)
    (throw (Exception. (str "Expected collection of length: " c)))))

(defn -mandate-min-length [v c]
  (when-not (>= (count v) c)
    (throw (Exception. (str "Expected collection of minimum length: " c)))))

(defn -mandate-first [a b]
  (when-not (= (first a) b)
    (throw (Exception. (str "Expected: " b ", got: " (first a))))))

(defn munge-where [w]
  (-munge-expr w))

(defn -munge-op [[kw op args & maybe-metadata :as input]]
  (-mandate-first input :op)
  (when-not (or (= :apply op)
                (string? op))
    (throw (Exception. "Your operator must be either a string or :apply")))
  (-mandate-vector args)
  (let [meta (if (seq maybe-metadata)
               (first maybe-metadata)
               {})]
    (if (or (= :apply op)
            (= "apply" op))
      (let [[func fargs] args]
        (-mandate-string func)
        (-mandate-min-length fargs 1)
        [:op :apply [func (map -munge-expr fargs)] meta])
      [:op op (map -munge-expr args) meta])))

;; [:table name]
(defn -munge-table [[kw name :as input]]
  (-mandate-first input :table)
  (-mandate-string name)
  [:table name])

(defn -munge-join-meta [m]
  (let [type (:type m)
        on (:on m)]
    (cond
     (= type :cross) (do
                       (when (seq on)
                         (throw (Exception. "Cross joins cannot have conditions")))
                       {:type :cross})
     (not= -1 (.indexOf
               [:left :right :inner]
               type)) (do
                        (when-not (seq on)
                          (throw (Exception. (str (name type) " joins require a condition"))))
                        {:type type
                         :on (-munge-expr on)})
     (nil? type) (do
                   (when-not on
                     (throw (Exception. (str "Presumed you wanted an inner join since you didn't specify, but that requires a condition"))))
                   {:type :inner
                    :on (-munge-expr on)})
     :default (u/unexpected-err m))))

;; LATERAL [:query {}]
;; LATERAL [:alias [:query {}] alias]
(defn -munge-lateral [[kw [head & tail :as thing] :as input]]
  (-mandate-first input :lateral)
  (if (= head :alias)
    [:lateral (-munge-alias thing :query)]
    (throw (Exception. "LATERAL subqueries MUST be aliased"))))

;; [:join t1 t2 {:type :left :on expr}]
;; TODO:
;; - NATURAL JOIN
;; - USING (a,b,c)
(defn -munge-join [[kw t1 [t2type & t2args :as t2] meta :as input]]
  (-mandate-first input :join)
  ;; TODO: are laterals required to be aliased?
  (when (and (= t2type :lateral) (= (:type meta) :right))
    (throw (Exception. "Laterals cannot be joined to by a right join. Syntactically it would be valid, but either it would break your query or you should use an ordinary subquery")))
    [:join (-munge-from-token t1) (-munge-from-token t2) (-munge-join-meta meta)])

(defn -munge-alias [[kw [type & rest :as item] alias :as input] context]
  (-mandate-first input :alias)
  (-mandate-string alias)
  (let [next (condp = context
               :from (condp = type
                       :table (-munge-table item)
                       :query (-munge-query item)
                       :raw (-munge-raw item);; http://www.postgresql.org/docs/9.3/static/queries-table-expressions.html#QUERIES-FROM
                       :tablefunc (throw (Exception. "Don't support table function FROM sources yet"))
                       (u/unexpected-err item))
               :query (if (= type :query)
                        (-munge-query item)
                        (u/unexpected-err item))
               (throw (Exception. (str "Unknown alias context: " context))))]           
    [:alias next alias]))

(defn -munge-from-token [[type & attrs :as f]]
  (condp = type
    :table (-munge-table f)
    :join (-munge-join f)
    :alias (-munge-alias f :from)
    :lateral (-munge-lateral f)
    ;; http://www.postgresql.org/docs/9.3/static/queries-table-expressions.html#QUERIES-FROM
    :tablefunc (throw (Exception. "Don't support table function FROM sources yet"))
    (u/unexpected-err f)))

;; This may have to be modified to support contexts since
;; Not everything should be permissible in e.g. an UPDATE
;; At least I think so, need to play around.
(defn munge-from [f]
  (-mandate-vector f)
  (map -munge-from-token f))

;; FIXME. One of the things we can do here is check that 
;; the user hasn't added types of clause that are unfit for the
;; desired query type
(defn -munge-select-query [q] q)
(defn -munge-update-query [q] q)
(defn -munge-delete-query [q] q)
(defn -munge-insert-query [q] q)

;; [:query {}]
;; Realistically this is more verification than munging.
(defn -munge-query [[kw arg :as q]]
  (-mandate-first q :query)
  (-mandate-map arg)
  (let [type (:type arg)
        next (condp = type
               :select (-munge-select-query arg)
               :update (-munge-update-query arg)
               :delete (-munge-delete-query arg)
               :insert (-munge-insert-query arg)
               (throw (Exception. (str "I don't recognise your query type: " type))))]
    [:query next]))

(defn -munge-group-by [g]
  (throw (Exception. "Group by clauses coming soon")))
(defn -munge-having [h]
  (throw (Exception. "Having clauses coming soon")))
(defn -munge-window [w]
  (throw (Exception. "Don't support window clauses yet")))

;; Most people are familiar with limit/offset being integer constants
;; but you can provide an arbitrary expression provided this resolves
;; to an integer. We should probably do some further analysis to
;; enforce at the very least not using logical ops unless there's
;; a cast or something. Perhaps we should upgrade casts to be their
;; own type. Cost would be more cases to handle.
(defn -munge-limit [[kw expr :as l]]
  (-mandate-first l :limit)
  (if (u/is-integer? expr)
    [:limit [:value expr]]
    [:limit (-munge-expr expr)]))

(defn -munge-offset [[kw expr :as o]]
  (-mandate-first o :offset)
  (if (u/is-integer? expr)
    [:offset [:value expr]]
    [:offset (-munge-expr expr)]))

(defn munge-insert-fields [f]
  (-mandate-vector f)
  (map -munge-set-atom f))
(defn munge-select-fields [f]
  (-mandate-vector f)
  (map (fn [field]
         (if (u/is-string? field)
           (-munge-identifier field)
           (let [[type & args] field]
             (condp = type
               :identifier (-munge-identifier field)
               :raw (-munge-raw field)
               (u/unexpected-err field)))))
       f))

(defn munge-update-fields [f])

(defn -munge-for [f]
  (throw (Exception. "Don't support for clauses yet")))
(defn -munge-with [w]
  (throw (Exception. "Don't support with modifiers yet")))
;; Slightly verbose name, I'll grant. Refers to referencing with
;; queries from a for clause
(defn -munge-with-query-in-from [w]
  (throw (Exception. "Don't support with-query FROM sources yet")))

(defn munge-select-meta [maybe-meta]
  (if (seq maybe-meta)
    (let [meta (first maybe-meta)
          distinct (:distinct meta)
          on (:on meta)]
      (if (not (nil? distinct))
        (do (-mandate-bool distinct)
            (if (seq on)
              (let [expr (-munge-expr on)]
                {:distinct distinct :on on})
              {:distinct distinct}))
        (do (when (seq on)
              (throw (Exception. "You can't have an on clause on a select unless it's also distinct. SELECT DISTINCT ON...")))
            {})))
    {}))

(defn -munge-identifier [a]
  (cond
   (string? a) [:identifier [a]]
   (veclike? a) (let [[head & tail] a]
                 (cond
                  (not (seq a)) (u/unexpected-err a)
                  (= a [:identifier]) (u/unexpected-err a)
                  (= head :identifier) (do
                                         (if (= (count tail) 1)
                                           (let [fst (first tail)]
                                             (if (veclike? fst)
                                               (if (every? string? fst)
                                                 [:identifier fst]
                                                 (u/unexpected-err fst))
                                               (if (string? fst)
                                                 [:identifier [fst]]
                                                 (u/unexpected-err fst))))
                                           (if (every? string? tail)
                                             [:identifier tail]
                                             (u/unexpected-err a))))
                  (every? string? a) [:identifier a]
                  :default (u/unexpected-err a)))
   :default (u/unexpected-err a)))
                 
                 
(defn -munge-value [v]
  (if (u/is-basic-value? v)
    [:value v]
    (do
      (-mandate-vector v)
      (-mandate-length v 2)
      (-mandate-first v :value)
      v)))

(defn -munge-raw [r]
  (-mandate-vector r)
  (-mandate-length r 2)
  (-mandate-string (second r))
  (-mandate-first r :raw)
  r)

(defn -munge-placeholder [p]
  (-mandate-vector p)
  (-mandate-length p 1)
  (-mandate-first p :placeholder)
  p)

(defn -munge-expr [a]
  (if (u/is-basic-value? a)
    (-munge-value a)
    (do (-mandate-vector a)
        (-mandate-min-length a 1)
        (let [[head & tail] a]
          (cond
           (= head :placeholder) (-munge-placeholder a)
           (= head :value) (-munge-value a)
           (= head :identifier) (-munge-identifier a)
           (= head :op) (-munge-op a)
           (= head :raw) (-munge-raw a)
           :default (u/unexpected-err a))))))

(defn -munge-set-atom [a]
  (cond
   (string? a) [(-munge-identifier a) [:placeholder]]
   (veclike? a) (let [[head & tail] a]
                 (cond
                  (= head :identifier) (let [i (-munge-identifier a)]
                                         [i [:placeholder]])
                  ;; [[] & tail]
                  ;; You can put a single string in head if you like
                  ;; and this makes it unambiguous and so work right
                  (veclike? head) (do
                                  (-mandate-length tail 1)
                                  (let [head2 (first tail)]
                                    [(-munge-identifier head)
                                     (-munge-expr head2)]))
                  :default (u/unexpected-err a)))
   :default (u/unexpected-err a)))

(defn munge-set [s]
  (-mandate-vector s)
  (-mandate-min-length s 1)
  (map -munge-set-atom s))

(defn -munge-fetch [f]
  (throw (Exception. "Don't support FETCH yet")))
