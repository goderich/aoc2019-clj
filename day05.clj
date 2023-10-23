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
