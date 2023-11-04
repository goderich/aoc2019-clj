(ns lib.helpers)

(defn queue
  "Helper function to create queues.
  Works with both a single int and a vector."
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([arg]
   (if (seqable? arg)
     (reduce conj clojure.lang.PersistentQueue/EMPTY arg)
     (conj clojure.lang.PersistentQueue/EMPTY arg))))
