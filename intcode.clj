(ns intcode
  (:require [helpers :refer [queue]]))

(defn- get-code [state]
  (-> (:pos state)
      ((:vec state))
      (rem 100)))

(defn- int->digits [n]
  (map #(Character/digit % 10) (str n)))

(defn- move-fwd
  "Move pointer forward by N steps."
  [state n]
  (update state :pos #(+ % n)))

(defn- get-value
  "Get a value from the vector based on mode.
  Positional mode (0): extract value from position at index.
  Immediate mode (1): extract value from current position."
  [{v :vec, pos :pos} index]
  (let [mode (-> pos v int->digits reverse (nth (inc index) 0))]
    (if (zero? mode)
      (v (v (+ pos index)))
      (v (+ pos index)))))

(defmulti ^:private run-code
  "Execute a single code instruction and move the pointer.
  This is a dispatch function that calls different methods
  based on the value of the instruction code."
  get-code)

(defmethod run-code 1 [{v :vec, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (+ x y)
        new-pos (v (+ pos 3))]
    (-> state
        (assoc-in [:vec new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 2 [{v :vec, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (* x y)
        new-pos (v (+ pos 3))]
    (-> state
        (assoc-in [:vec new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 3 [{v :vec, pos :pos, in :input :as state}]
  (let [i (peek in)]
    (-> state
        (assoc-in [:vec (v (inc pos))] i)
        (update :input pop)
        (move-fwd 2))))

(defmethod run-code 4 [state]
  (let [i (get-value state 1)]
    (-> state
        (update :output #(conj % i))
        (move-fwd 2))))

(defmethod run-code 5 [state]
  (let [x (get-value state 1)]
    (if (not (zero? x))
      (assoc state :pos (get-value state 2))
      (move-fwd state 3))))

(defmethod run-code 6 [state]
  (let [x (get-value state 1)]
    (if (zero? x)
      (assoc state :pos (get-value state 2))
      (move-fwd state 3))))

(defmethod run-code 7 [{v :vec, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (if (< x y) 1 0)
        new-pos (v (+ pos 3))]
    (-> state
        (assoc-in [:vec new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 8 [{v :vec, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (if (= x y) 1 0)
        new-pos (v (+ pos 3))]
    (-> state
        (assoc-in [:vec new-pos] new-val)
        (move-fwd 4))))

(defn initialize
  "Create a map from a vector and optional arguments."
  ([v] (initialize v nil))
  ([v input]
   {:vec v, :pos 0, :input (queue input), :output []}))

(defn run
  "Main intcode fuction.
  Halts on either code 99, or upon receiving an output."
  [m]
  {:pre [(map? m)]}
  (loop [state m]
    (cond
      (= 99 (get-code state)) state
      (seq (:output state)) state
      :else (recur (run-code state)))))

(defn run-until-halt
  "Run instructions, accumulating outputs.
  Do not halt on output, but exit only on code 99.
  This is required for day 5 part 1."
  [m]
  {:pre [(map? m)]}
  (loop [state m]
    (cond
      (= 99 (get-code state)) state
      :else (recur (run-code state)))))
