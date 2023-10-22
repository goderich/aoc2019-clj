(ns test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [intcode]))

(deftest day02
  (testing "Basic functionality"
    (is
     (= (:vec (intcode/run [1,9,10,3,2,3,11,0,99,30,40,50]))
        [3500 9 10 70 2 3 11 0 99 30 40 50]))))

(defn- output [state]
  (first (:output state)))

(deftest day05
  (testing "Testing modes"
    (is
     (= (:vec (intcode/run [1002,4,3,4,33]))
        [1002 4 3 4 99])))

  (testing "Testing optcodes 3 and 4"
    (is
     (= 8 (output (intcode/run [3,0,4,0,99] 8)))))

  (testing "Testing jumps"

    (testing "Position mode"
      (let [v [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]

      (is (zero? (output (intcode/run v 0))))
      (is (= 1   (output (intcode/run v 3))))))

    (testing "Immediate mode"
      (let [v [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]

        (is (zero? (output (intcode/run v 0))))
        (is (= 1   (output (intcode/run v 8)))))))

  (testing "Larger input test"
    (let [v [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
             1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
             999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]

      (is (= 999  (output (intcode/run v 7))))
      (is (= 1000 (output (intcode/run v 8))))
      (is (= 1001 (output (intcode/run v 9)))))))

(def test-results
  (t/run-tests))

(let [{:keys [fail error]} test-results]
  (when (pos? (+ fail error))
    (System/exit 1)))
