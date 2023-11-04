(ns aoc.day02
  (:require [clojure.string :as str]
            [lib.intcode :as intcode]))

(def input
  (as-> (slurp "inputs/day02.txt") _
    (str/trim-newline _)
    (str/split _ #",")
    (mapv #(Integer/parseInt %) _)))

(defn- try-inputs [x y]
  (let [v (-> input
              (assoc 1 x)
              (assoc 2 y))]
    (-> v intcode/initialize intcode/run :vec first)))

;; part 1

(try-inputs 12 2)
;; => 5098658

;; part 2

(for [x (range 100)
      y (range 100)
      :when (= (try-inputs x y) 19690720)]
  (+ (* 100 x) y))
;; => (5064)
