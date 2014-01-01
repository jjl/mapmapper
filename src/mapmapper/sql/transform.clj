(ns mapmapper.sql.transform
  (require [mapmapper.util :as u]))

(declare -munge-set-atom)

(defn veclike? [v]
  (or (vector? v)
      (list? v)
      (seq? v)))

(defn -mandate-vector [v]
  (when-not (veclike? v)
    (throw (Exception. (str "Expected vector: " v)))))

(defn -mandate-length [v c]
  (when-not (= (count v) c)
    (throw (Exception. (str "Expected collection of length: " c)))))

(defn -mandate-min-length [v c]
  (when-not (>= (count v) c)
    (throw (Exception. (str "Expected collection of minimum length: " c)))))

(defn -mandate-string [i]
  (when-not (string? i)
    (throw (Exception. (str "Expected string: " i)))))

(defn -mandate-first [a b]
  (when-not (= (first a) b)
    (throw (Exception. (str "Expected: " b ", got: " (first a))))))

(defn munge-where [w] w)

(defn -munge-op [o] o)

(defn munge-from [f] f)

(defn munge-insert [i]
  (-mandate-vector i)
  (map -munge-set-atom i))
                  

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
  (-mandate-vector v)
  (-mandate-length v 2)
  (-mandate-first v :value)
  v)

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
  (-mandate-vector a)
  (-mandate-min-length a 1)
   (let [[head & tail] a]
     (cond
      (= head :placeholder) (-munge-placeholder a)
      (= head :value) (-munge-value a)
      (= head :identifier) (-munge-identifier a)
      (= head :op) (-munge-op a)
      (= head :raw) (-munge-raw a)
      :default (u/unexpected-err a))))

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

