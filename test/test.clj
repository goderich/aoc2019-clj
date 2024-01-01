(ns test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [lib.intcode :as intcode]
            [lib.helpers :refer [queue]]))

(defn- init-run
  ([v] (init-run v nil))
  ([v input] (intcode/run (intcode/initialize v input))))

(defn- output [m]
  (first (:output m)))

(deftest day02
  (testing "Basic functionality"
    (is
     (= (vals (:mem (init-run [1,9,10,3,2,3,11,0,99,30,40,50])))
        [3500 9 10 70 2 3 11 0 99 30 40 50]))))

(deftest day05
  (testing "Modes"
    (is
     (= (vals (:mem (init-run [1002,4,3,4,33])))
        [1002 4 3 4 99])))

  (testing "Optcodes 3 and 4"
    (is
     (= 8 (output (init-run [3,0,4,0,99] 8)))))

  (testing "Jumps"

    (testing "Position mode"
      (let [v [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]]

      (is (zero? (output (init-run v 0))))
      (is (= 1   (output (init-run v 3))))))

    (testing "Immediate mode"
      (let [v [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]]

        (is (zero? (output (init-run v 0))))
        (is (= 1   (output (init-run v 8)))))))

  (testing "Larger input test"
    (let [v [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
             1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
             999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]]

      (is (= 999  (output (init-run v 7))))
      (is (= 1000 (output (init-run v 8))))
      (is (= 1001 (output (init-run v 9)))))))

(deftest day07
  (testing "Simple looping"
    (is (= 43210
           (let [v [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]]
             (loop [amps (mapv #(intcode/initialize v %) [4 3 2 1 0])
                    input 0]
               (if (empty? amps)
                 input
                 (let [amp (update (first amps) :input #(conj % input))
                       output (first (:output (intcode/run amp)))]
                   (recur (rest amps) output))))))))

  (testing "Feedback loop"
    (is (= 139629729
           (let [v [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]]
             (loop [amps (queue (map #(intcode/initialize v %) [9 8 7 6 5]))
                    input 0]
               (let [amp (-> (peek amps)
                             (update :input #(conj % input))
                             (intcode/run))
                     output (first (:output amp))]
                 (if output
                   (recur (conj (pop amps) (update amp :output pop)) output)
                   input))))))))

(deftest day09
  (testing "Basic day 9 features"

    (testing "Relative mode"
      (let [v [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]]
        (is (= v
               (vals (:mem (intcode/run (intcode/initialize v))))))))

    (testing "Large memory"
      (let [v [104,1125899906842624,99]]
        (is (= 1125899906842624
               (-> v intcode/initialize intcode/run :output first)))))))

(def test-results
  (t/run-tests))

(let [{:keys [fail error]} test-results]
  (when (pos? (+ fail error))
    (System/exit 1)))
