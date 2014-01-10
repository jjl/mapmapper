(ns mapmapper.util.map
  (:require [mapmapper.util.set :as us]))

(defn without [thing to-go]
  (apply dissoc thing (seq to-go)))

(defn with-only [thing to-stay]
  (without thing (us/without (set (keys thing)) (set to-stay))))

(defn keywordify [before]
  (reduce #(assoc %1 (keyword %2)
                  (get before %2)) {} (keys before)))
  
