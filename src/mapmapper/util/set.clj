(ns mapmapper.util.set
  (require [clojure.set :as cset]))

(defn has-all? [item expected]
  (not (seq (cset/difference expected item))))

(defn has-any? [item expected]
  (seq (cset/intersection item expected)))

(defn has-only? [item expected]
  (cset/subset? item expected))

(defn without [item to-go]
  (cset/difference item to-go))

(defn without-except [item to-stay]
  (cset/difference to-stay item))
