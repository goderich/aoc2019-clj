(ns aoc.day07
  (:require [clojure.string :as str]
            [lib.intcode :as intcode]
            [lib.helpers :refer [queue]]
            [babashka.deps :as deps]))

(def program
  (as-> (slurp "inputs/day07.txt") it
    (str/trim-newline it)
    (str/split it #",")
    (mapv #(Integer/parseInt %) it)))

(deps/add-deps '{:deps {org.clojure/math.combinatorics {:mvn/version "0.2.0"}}})
(require '[clojure.math.combinatorics :as combo])

;; part 1

(->>
 (for [phase (combo/permutations [0 1 2 3 4])]
   (loop [amps (mapv #(intcode/initialize program %) phase)
          input 0]
     (if (empty? amps)
       input
       (let [amp (-> (first amps)
                     (update :input #(conj % input))
                     (intcode/run))
             output (first (:output amp))]
         (recur (rest amps) output)))))

 (apply max))
;; => 255840

;; part 2

(->>
 (for [phase (combo/permutations [5 6 7 8 9])]
   (loop [amps (queue (map #(intcode/initialize program %) phase))
         input 0]
    (let [amp (-> (peek amps)
                  (update :input #(conj % input))
                  (intcode/run))
          output (first (:output amp))]
      (if output
        (recur (conj (pop amps) (update amp :output pop)) output)
        input))))

 (apply max))
;; => 84088865
