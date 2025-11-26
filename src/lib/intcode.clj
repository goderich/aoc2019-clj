(ns lib.intcode
  (:require [lib.helpers :refer [queue int->digits]]))

(defn- get-code [state]
  (-> (:pos state)
      ((:mem state))
      (rem 100)))

(defn- move-fwd
  "Move pointer forward by N steps."
  [state n]
  (update state :pos + n))

(defn- get-value
  "Get a value from the intcode computer memory based on mode.
  Positional mode (0): extract value from position at index.
  Immediate mode (1): extract value from current position.
  Relative mode (2): extract value relative to a base (see day 9)."
  [{m :mem, pos :pos, rb :relbase} index]
  (let [mode (-> pos m int->digits reverse (nth (inc index) 0))
        num (m (+ pos index))] ; numeric value at address
    (case mode
      0 (m num 0)
      1 num
      2 (m (+ rb num) 0))))

(defn- get-position
  "Get a position index in the intcode computer memory based on mode,
  for instructions that write to the memory.
  Positional mode (0): extract address from position at index.
  Immediate mode: impossible (see day 5).
  Relative mode (2): address relative to a base (see day 9)."
  [{m :mem, pos :pos, rb :relbase} index]
  (let [mode (-> pos m int->digits reverse (nth (inc index) 0))
        num (m (+ pos index))] ; numeric value at address
    (case mode
      0 num
      1 (throw (Throwable. "Called immediate mode in positional argument."))
      2 (+ rb num))))

(defmulti ^:private run-code
  "Execute a single code instruction and move the pointer.
  This is a dispatch function that calls different methods
  based on the value of the instruction code."
  get-code)

(defmethod run-code 1 [{m :mem, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (+ x y)
        new-pos (get-position state 3)]
    (-> state
        (assoc-in [:mem new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 2 [{m :mem, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (* x y)
        new-pos (get-position state 3)]
    (-> state
        (assoc-in [:mem new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 3 [{m :mem, pos :pos, in :input :as state}]
  (let [x (peek in)
        pos (get-position state 1)]
    (-> state
        (assoc-in [:mem pos] x)
        (update :input pop)
        (move-fwd 2))))

(defmethod run-code 4 [state]
  (let [x (get-value state 1)]
    (-> state
        (update :output conj x)
        (move-fwd 2))))

(defmethod run-code 5 [state]
  (let [x (get-value state 1)]
    (if-not (zero? x)
      (assoc state :pos (get-value state 2))
      (move-fwd state 3))))

(defmethod run-code 6 [state]
  (let [x (get-value state 1)]
    (if (zero? x)
      (assoc state :pos (get-value state 2))
      (move-fwd state 3))))

(defmethod run-code 7 [{m :mem, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (if (< x y) 1 0)
        new-pos (get-position state 3)]
    (-> state
        (assoc-in [:mem new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 8 [{m :mem, pos :pos :as state}]
  (let [x (get-value state 1)
        y (get-value state 2)
        new-val (if (= x y) 1 0)
        new-pos (get-position state 3)]
    (-> state
        (assoc-in [:mem new-pos] new-val)
        (move-fwd 4))))

(defmethod run-code 9 [state]
  (let [x (get-value state 1)]
    (-> state
        (update :relbase + x)
        (move-fwd 2))))

(defn initialize
  "Create a map from a vector and optional arguments.
  Use a sorted map instead of a vector to represent
  the intcode computer memory."
  ([v] (initialize v nil))
  ([v input]
   (let [v (map bigint v)]
     {:mem (into (sorted-map) (zipmap (range) v)),
      :pos 0,
      :input (queue input),
      :output [],
      :relbase 0})))

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
