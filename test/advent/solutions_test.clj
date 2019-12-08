(ns advent.solutions-test
  (:require [advent.solutions :as solutions]
            [clojure.test :refer [deftest testing is]]))

(deftest day-1-test
  (testing "day-1-part-1"
    (is (= 3325342
           (solutions/day-1-part-1))))
  (testing "day-1-part-2"
    (is (= 4985158
           (solutions/day-1-part-2)))))

(deftest day-2-test
  (testing "part-1"
    (testing "examples"
      (is (= {:memory [2 0 0 0 99], :ptr 4, :running? false :inputs nil}
             (solutions/interpret (solutions/machine [[1 0 0 0 99] 0 true]))))
      (is (= {:memory [2 3 0 6 99], :ptr 4, :running? false :inputs nil}
             (solutions/interpret (solutions/machine [[2 3 0 3 99] 0 true]))))
      (is (= {:memory [2 4 4 5 99 9801], :ptr 4, :running? false :inputs nil}
             (solutions/interpret (solutions/machine [[2 4 4 5 99 0] 0 true]))))
      (is (= {:memory [30 1 1 4 2 5 6 0 99], :ptr 8, :running? false :inputs nil}
             (solutions/interpret (solutions/machine [[1 1 1 4 99 5 6 0 99] 0 true])))))
    (testing "1202 program alarm"
      (is (= 6327510
             (let [{:keys [memory]} (solutions/interpret (solutions/machine [(solutions/set-memory-for-1202 (solutions/computer-memory)) 0 true]))]
               (first memory))))))
  (testing "part-2"
    (is (= [41 12]
           (solutions/matching-inputs (solutions/computer-memory) 19690720)))
    (is (= 4112
           (let [[noun verb] (solutions/matching-inputs (solutions/computer-memory) 19690720)]
             (+ verb (* noun 100)))))))

(deftest day-3-part-1
  (testing "part-1"
    (is (= 6
           (solutions/closest-manhattan-to-center [(solutions/parse-wire "R8,U5,L5,D3")
                                                   (solutions/parse-wire "U7,R6,D4,L4")])))

    (is (= 159
           (solutions/closest-manhattan-to-center [(solutions/parse-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                                                   (solutions/parse-wire "U62,R66,U55,R34,D71,R55,D58,R83")])))

    (is (= 135
           (solutions/closest-manhattan-to-center (map solutions/parse-wire ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                                                             "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))))

    (is (= 399
           (solutions/closest-manhattan-to-center (solutions/wires)))))
  (testing "part-2"
    (is (= 30
           (solutions/closest-length-to-center (map solutions/parse-wire ["R8,U5,L5,D3" "U7,R6,D4,L4"]))))
    (is (= 610
           (solutions/closest-length-to-center (map solutions/parse-wire ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                                                          "U62,R66,U55,R34,D71,R55,D58,R83"]))))

    (is (= 610
           (solutions/closest-length-to-center (map solutions/parse-wire ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                                                          "U62,R66,U55,R34,D71,R55,D58,R83"]))))

    (is (= 410
           (solutions/closest-length-to-center (map solutions/parse-wire ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                                                          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))))    

    (is (= 15678
           (solutions/closest-length-to-center (solutions/wires))))))

(deftest day-4
  (is (= 544
         (count (solutions/passwords-part1 356261 846303))))
  (is (= 334
         (count (solutions/passwords-part2 356261 846303)))))


(deftest day-5-parameter-modes
  (testing "mode parsing"
    (is (= [3 2 1]
           (solutions/parameter-modes [(Long/parseLong "12300")] 0))))
  (testing "immediate mode"
    (is (= {:memory [5 2 3 0 99], :ptr 4, :running? false :inputs nil}
           (solutions/interpret (solutions/machine [[1101 2 3 0 99] 0 true])))))
  (testing "code"
    (is (= [0 0 0 0 0 0 0 0 0 15386262]
           (:outputs (solutions/interpret (solutions/machine [(solutions/computer-memory-day-5)
                                                              0
                                                              true])
                                          [1])))))
  (testing "equals indirect"
    (is (= [1]
           (:outputs (solutions/interpret {:memory [3,9,8,9,10,9,4,9,99,-1,8]
                                           :ptr 0
                                           :running? true}
                                          [8]))))
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,9,8,9,10,9,4,9,99,-1,8]
                                           :ptr 0
                                           :running? true}
                                          [7]))))
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,9,8,9,10,9,4,9,99,-1,8]
                                           :ptr 0
                                           :running? true}
                                          [9])))))

  (testing "less-than indirect"
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,9,7,9,10,9,4,9,99,-1,8]
                                           :ptr 0
                                           :running? true}
                                          [8]))))
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,9,7,9,10,9,4,9,99,-1,8]
                                           :ptr 0
                                           :running? true}
                                          [9]))))
    (is (= [1]
           (:outputs (solutions/interpret {:memory [3,9,7,9,10,9,4,9,99,-1,8]
                                           :ptr 0
                                           :running? true}
                                          [7])))))

  (testing "equals immediate"
    (is (= [1]
           (:outputs (solutions/interpret {:memory [3,3,1108,-1,8,3,4,3,99]
                                           :ptr 0
                                           :running? true}
                                          [8]))))
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,3,1108,-1,8,3,4,3,99]
                                           :ptr 0
                                           :running? true}
                                          [7])))))

  (testing "less-than immediate"
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,3,1107,-1,8,3,4,3,99]
                                           :ptr 0
                                           :running? true}
                                          [8]))))
    (is (= [1]
           (:outputs (solutions/interpret {:memory [3,3,1107,-1,8,3,4,3,99]
                                           :ptr 0
                                           :running? true}
                                          [7])))))

  (testing "jump indirect"
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
                                           :ptr 0
                                           :running? true}
                                          [0]))))
    (is (= [1]
           (:outputs (solutions/interpret {:memory [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
                                           :ptr 0
                                           :running? true}
                                          [2]))))

    )

  (testing "jump immediate"
    (is (= [0]
           (:outputs (solutions/interpret {:memory [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
                                           :ptr 0
                                           :running? true}
                                          [0]))))
    (is (= [1]
           (:outputs (solutions/interpret {:memory [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
                                           :ptr 0
                                           :running? true}
                                          [2])))))
  (is (= [999]
         (:outputs (solutions/interpret {:memory [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                                  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                                  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                                         :ptr 0
                                         :running? true}
                                        [1]))))
  (is (= [1000]
         (:outputs (solutions/interpret {:memory [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                                  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                                  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                                         :ptr 0
                                         :running? true}
                                        [8]))))
  (is (= [1001]
         (:outputs (solutions/interpret {:memory [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                                  1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                                  999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
                                         :ptr 0
                                         :running? true}
                                        [10]))))

  (testing "day-5-part-2"
    (is (= [10376124]
           (:outputs (solutions/interpret {:memory   (solutions/computer-memory-day-5)
                                           :ptr      0
                                           :running? true}
                                          [5]))))))



