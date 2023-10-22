(ns day01
  (:require [clojure.string :as str]))

(def input (map #(Integer/parseInt %) (str/split-lines (slurp "inputs/day01.txt"))))

;; part 1

(defn- calc [n]
  (-> n
      (/ 3)
      (Math/floor)
      (- 2)
      int))

(reduce + (map calc input))
;; => 3337766

;; part 2

(defn- calc-rec [n]
  (loop [fuel (calc n)
         acc 0]
    (if (pos? fuel)
      (recur (calc fuel) (+ acc fuel))
      acc)))

(reduce + (map calc-rec input))
;; => 5003788
