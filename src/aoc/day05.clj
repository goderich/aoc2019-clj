(ns aoc.day05
  (:require [clojure.string :as str]
            [lib.intcode :as intcode]))

(def input
  (as-> (slurp "inputs/day05.txt") it
    (str/trim-newline it)
    (str/split it #",")
    (mapv #(Integer/parseInt %) it)))

;; part 1

(-> input
    (intcode/initialize 1)
    intcode/run-until-halt
    :output
    last)
;; => 7259358

;; part 2

(-> input
    (intcode/initialize 5)
    intcode/run
    :output
    first)
;; => 11826654
