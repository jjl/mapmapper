(ns mapmapper.sql
  (:require [clojure.string :as string]
            [mapmapper.util :as u]
            [mapmapper.sql.transform :as t]
            [mapmapper.sql.render :as r])
  (:refer-clojure :except [set]))

(declare -render-expr)


(defn -alias-table [table aliases]
  (let [next (str "t" (-> aliases keys count))
        new-aliases (assoc aliases next table)]
    [next new-aliases]))

(defn insert [table]
  {:type :insert
   :table table})

(defn delete [table]
  {:type :delete
   :table table})

(defn select [& maybe-meta]
  {:type :select
   :meta (t/munge-select-meta maybe-meta)})

(defn update [table]
  {:type :update
   :table table})

(defn set [query fields]
  (assoc query :set (t/munge-set fields)))

(defn fields [query fields]
  (let [type (:type query)]
    (condp = type
      :insert (assoc query :fields (t/munge-insert-fields fields))
      :select (assoc query :fields (t/munge-select-fields fields))
      :update (assoc query :fields (t/munge-update-fields fields))
      (throw (Exception. "Only insert, select and update can have fields")))))

(def where-atom-types
  [:identifier :value :op])

(defn where [query w]
  (assoc query :where (t/munge-where w)))

(defn from [query f]
  (assoc query :from (t/munge-from f)))

(defn render [query]
  (let [type (:type query)]
    (cond (= type :insert) (r/render-insert query)
          (= type :select) (r/render-select query)
          (= type :update) (r/render-update query)
          (= type :delete) (r/render-delete query))))

