(ns day05
  (:require [clojure.string :as str]
            [intcode]))

(def input
  (as-> (slurp "inputs/day05.txt") _
    (str/trim-newline _)
    (str/split _ #",")
    (mapv #(Integer/parseInt %) _)))

;; part 1

(-> input
    (intcode/run 1)
    :output
    last)
;; => 7259358

;; part 2

(-> input
    (intcode/run 5)
    :output
    first)
;; => 11826654
