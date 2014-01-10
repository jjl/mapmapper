(ns mapmapper.djangly.defs
  (:require [mapmapper.util :as u]
            [mapmapper.util.map :as um]
            [mapmapper.djangly.transform :as t]
            [flatland.ordered.map :refer [ordered-map]]))

(defn make-unique [& cols]
  {:type :unique
   :cols cols})

(defn make-index [& cols]
  {:type :index
   :cols cols})

(defn make-pk [& cols]
  {:type :pk
   :cols cols})

(defn find-pk [cols constraints]
  (let [col-pk (filter #(and (contains? (% :attrs) :pk)
                             ((% :attrs) :pk)) cols)
        constraints-pk (filter #(= (% :type) :pk) constraints)]
    (when (> (+ (count constraints-pk)
                (count col-pk)) 1)
      (throw (IllegalArgumentException. "There can be only one primary key")))
    (when (= 0 (+ (count constraints-pk)
                  (count col-pk)))
      (throw (IllegalArgumentException. "I can't handle PK-less tables yet. Sorry.")))
    (if (seq col-pk)
      (list (:name (first col-pk)))
      (:cols (first constraints-pk)))))

(defn -make-cols-gatherer [attribute]
  (fn [cols]
    (let [filtered (filter #(get (get % :attrs {}) attribute false) cols)]
      (map #(list (% :name)) filtered))))
(defn -make-csts-gatherer [attribute]
  (fn [csts]
    (let [filtered (filter #(= (get % :type nil) :attribute) csts)]
      (map :cols filtered))))

(def -gather-unique-cols (-make-cols-gatherer :unique))
(def -gather-index-cols (-make-cols-gatherer :index))
(def -gather-fk-cols (-make-cols-gatherer :fk))
(def -gather-unique-csts (-make-csts-gatherer :unique))
(def -gather-index-csts (-make-csts-gatherer :index))
(def -gather-fk-csts (-make-csts-gatherer :fk))

(defn -merge-indices [& items]
  (apply concat items))

(defn -shrink-cols [cols]
  (let [permitted [:length :precision :default]]
    (reduce (fn [acc col]
              (let [attrs (col :attrs)]
                (let [reduced (um/with-only attrs permitted)]
                  (let [new (dissoc (merge col reduced)
                                    :attrs :i-am-a :name)]
                    (assoc acc (col :name) new))))) (ordered-map) cols)))

(defn -shrink-csts [csts]
  ;; TODO: what should a shrunk constraint look like?
  csts)

(defn -process-table [name cols csts]
  (let [pk (find-pk cols csts)
        uniques (-merge-indices (-gather-unique-cols cols)
                                (-gather-unique-csts csts))
        indices (-merge-indices (-gather-index-cols cols)
                                (-gather-index-csts csts))
        fks (-merge-indices (-gather-fk-cols cols)
                            (-gather-fk-csts csts))
        shrunk-cols (-shrink-cols cols)
        shrunk-csts (-shrink-csts csts)]
    {:pk pk
     :fks fks
     :uniques uniques
     :indices indices
     :cols shrunk-cols
     :constraints shrunk-csts}))

(defn make-table [-name & maybes]
  (let [{:keys [name cols constraints]} (t/munge-table -name maybes)]
    (-process-table name cols constraints)))
