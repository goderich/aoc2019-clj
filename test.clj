(ns test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [intcode]))

(defn- init-run
  ([v] (init-run v nil))
  ([v input] (intcode/run (intcode/initialize v input))))

(deftest day02
  (testing "Basic functionality"
    (is
     (= (:vec (init-run [1,9,10,3,2,3,11,0,99,30,40,50]))
        [3500 9 10 70 2 3 11 0 99 30 40 50]))))

(deftest day05
  (testing "Modes"
    (is
     (= (:vec (init-run [1002,4,3,4,33]))
        [1002 4 3 4 99])))

  (testing "Optcodes 3 and 4"
    (is
     (= 8 (:output (init-run [3,0,4,0,99] 8)))))

  (testing "Jumps"

    (testing "Position mode"
      (let [v [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]

      (is (zero? (:output (init-run v 0))))
      (is (= 1   (:output (init-run v 3))))))

    (testing "Immediate mode"
      (let [v [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]

        (is (zero? (:output (init-run v 0))))
        (is (= 1   (:output (init-run v 8)))))))

  (testing "Larger input test"
    (let [v [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
             1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
             999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]

      (is (= 999  (:output (init-run v 7))))
      (is (= 1000 (:output (init-run v 8))))
      (is (= 1001 (:output (init-run v 9)))))))

(def test-results
  (t/run-tests))

(let [{:keys [fail error]} test-results]
  (when (pos? (+ fail error))
    (System/exit 1)))
