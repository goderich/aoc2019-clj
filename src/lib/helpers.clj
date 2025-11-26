(ns lib.helpers
  (:require [clojure.string :as str]))

(defn queue
  "Helper function to create queues.
  Works with both a single int and a vector."
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([arg]
   (if (seqable? arg)
     (reduce conj clojure.lang.PersistentQueue/EMPTY arg)
     (conj clojure.lang.PersistentQueue/EMPTY arg))))

(defn int->digits [n]
  (map #(Character/digit % 10) (str n)))

(defn read-program [f]
  (as-> (slurp f) it
    (str/trim-newline it)
    (str/split it #",")
    (mapv #(Integer/parseInt %) it)))
