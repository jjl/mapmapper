(ns mapmapper.util)

(defn intersperse [raw item]
  (let [l-init (butlast raw)
        l-last (last raw)]
    (concat (mapcat #(vector % item) l-init)
            [l-last])))

(defn is-integer? [item]
  (or (instance? java.lang.Long item)
      (instance? java.lang.Integer item)))

(defn is-float? [item]
  (or (instance? java.lang.Double item)
      (instance? java.lang.Float item)))

(defn is-numeric? [item]
  (or (is-integer? item)
      (is-float? item)))

(defn is-boolean? [item]
  (instance? java.lang.Boolean item))

(defn is-string? [item]
  (instance? java.lang.String item))

(defn is-basic-value? [item]
  (or (is-numeric? item)
      (is-boolean? item)
      (is-string? item)))

(defn unexpected-err [v]
  (throw (Exception. (str "Unexpected data: " v))))

