(ns aoc.day03
  (:require [clojure.string :as str]))

(let [input (str/split-lines (slurp "inputs/day03.txt"))]
  (def wire1 (first input))
  (def wire2 (second input)))

(defn- run-instruction [wire [dir steps]]
  (let [nums (range 1 (inc steps))
        coords
        (case dir
          "R" (map #(update (:pos wire) :x + %) nums)
          "L" (map #(update (:pos wire) :x - %) nums)
          "U" (map #(update (:pos wire) :y + %) nums)
          "D" (map #(update (:pos wire) :y - %) nums))]
    (-> wire
        (update :path into coords)
        (assoc :pos (last coords)))))

(defn- parse-path [str]
  (map
   (fn [[_ dir num]] [dir (Integer/parseInt num)])
   (re-seq #"(\w)(\d+),?" str)))

(defn- wire-path [str]
  (reduce run-instruction
          {:pos {:x 0 :y 0} :path []}
          (parse-path str)))

;; saving paths and intersections
;; for use in both parts of the problem
(def path1 (-> wire1 wire-path :path))
(def path2 (-> wire2 wire-path :path))
(def intersections
  (clojure.set/intersection (set path1) (set path2)))

;; part 1

(apply min (map #(+ (abs (:x %)) (abs (:y %))) intersections))
;; => 1195

;; part 2

(defn- combined-steps [goal]
  (+ (inc (count (take-while #(not= % goal) path1)))
     (inc (count (take-while #(not= % goal) path2)))))

(apply min (map combined-steps intersections))
;; => 91518
