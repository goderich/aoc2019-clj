(ns aoc.day09
  (:require [clojure.string :as str]
            [lib.intcode :as intcode]
            [lib.helpers :refer [read-program]]))

(def input (read-program "inputs/day09.txt"))

(-> input
    (intcode/initialize 1)
    intcode/run
    (dissoc :mem)
    :output
    first)
;; => 2745604242N

(-> input
    (intcode/initialize 2)
    intcode/run
    (dissoc :mem)
    :output
    first)
;; => 51135N
