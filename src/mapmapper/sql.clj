(ns mapmapper.sql
  (:require [clojure.string :as string]))

(defn -alias-table [table aliases]
  (let [next (str "t" (-> aliases keys count))
        new-aliases (assoc aliases next table)]
    [next new-aliases]))

(defn -quote-identifier [& pieces]
  (str \" (string/join "\".\"" pieces) \"))

(defn -generate-placeholders [count]
  (string/join " " (repeat count "?")))

(defn -qualified-names [table cols]
  (string/join ", " (map #(-quote-identifier table %) cols)))

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
  "not implemented")

(defn generate [query]
  (let [type (:type query)]
    (cond (= type :insert) (-generate-insert query)
          (= type :select) (-generate-select query)
          (= type :update) (-generate-update query)
          (= type :delete) (-generate-delete query))))

  
  
  
  
