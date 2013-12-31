(ns mapmapper.util)

(defn intersperse [raw item]
  (let [l-init (butlast raw)
        l-last (last raw)]
    (concat (mapcat #(vector % item) l-init)
            [l-last])))

(defn is-numeric-type? [item]
  (or (instance? java.lang.Long item)
      (instance? java.lang.Integer item)
      (instance? java.lang.Double item)
      (instance? java.lang.Float item)))

(defn is-string-type? [item]
  (instance? java.lang.String item))
