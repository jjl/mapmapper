(ns mapmapper.sql.transform
  (require [mapmapper.util :as u]))

(declare -munge-set-atom)

(defn -mandate-vector [v]
  (when-not (or (vector? v)
                (list? v)
                (seq? v))
    (throw (Exception. (str "Expected vector: " v)))))

(defn -mandate-length [v c]
  (when-not (= (count v) c)
    (throw (Exception. (str "Expected collection of length: " c)))))

(defn -mandate-min-length [v c]
  (when-not (>= (count v) c)
    (throw (Exception. (str "Expected collection of minimum length: " c)))))

(defn munge-where [w] w)

(defn -munge-op [o] o)

(defn munge-from [f] f)

(defn munge-insert [i]
  (-mandate-vector i)
  (map -munge-set-atom i))
                  

(defn -munge-identifier [a]
  (cond
   (string? a) [:identifier [a]]
   (vector? a) (let [[head & tail] a]
                 (cond
                  (= head :identifier) (do
                                         (if (= (count tail) 1)
                                           (let [fst (first tail)]
                                             (if (vector? fst)
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
  (-mandate-vector v)
  (-mandate-length v 2)
  (when-not (= (first v) :value)
    (throw (Exception. (str "Invalid value: " v))))
  v)

(defn -munge-set-value [a]
  (cond
   (= a [:placeholder]) a
   (vector? a) (let [[head & tail] a]
                 (cond
                  (= head :value) (-munge-value a)
                  (= head :identifier) (-munge-identifier a)
                  (= head :op) (-munge-op a)
                  :default (u/unexpected-err a)))))

(defn -munge-set-atom [a]
  (cond
   (string? a) [(-munge-identifier a) [:placeholder]]
   (vector? a) (let [[head & tail] a]
                 (cond
                  (= head :identifier) (let [i (-munge-identifier a)]
                                         [i [:placeholder]])
                  (vector? head) (do
                                  (-mandate-length tail 1)
                                  (let [head2 (first tail)]
                                    [(-munge-identifier head)
                                     (-munge-set-value head2)]))
                  :default (u/unexpected-err)))
   :default (u/unexpected-err)))

(defn munge-set [s]
  (-mandate-vector s)
  (-mandate-min-length s 1)
  (map -munge-set-atom s))

