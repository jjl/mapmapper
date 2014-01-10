(ns mapmapper.djangly.transform
  (:require [mapmapper.util :as u]))

(defn munge-table [-name maybes]
  (let [t-name (cond
                (string? -name) -name
                (keyword? -name) (name -name)
                :default (u/unexpected-err -name))
        t-cols (if (seq maybes)
                 (first maybes)
                 [])
        t-constraints (if (and (seq maybes)
                               (seq (rest maybes)))
                        (first (rest maybes))
                        [])]
    {:name t-name
     :cols t-cols
     :constraints t-constraints}))
    
