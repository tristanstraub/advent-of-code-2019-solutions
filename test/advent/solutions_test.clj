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
      (is (= [[2 0 0 0 99] 4 false]
             (solutions/interpret [[1 0 0 0 99] 0 true])))
      (is (= [[2 3 0 6 99] 4 false]
             (solutions/interpret [[2 3 0 3 99] 0 true])))
      (is (= [[2 4 4 5 99 9801] 4 false]
             (solutions/interpret [[2 4 4 5 99 0] 0 true])))
      (is (= [[30 1 1 4 2 5 6 0 99] 8 false]
             (solutions/interpret [[1 1 1 4 99 5 6 0 99] 0 true]))))
    (testing "1202 program alarm"
      (is (= 6327510
             (let [[memory _ _] (solutions/interpret [(solutions/set-memory-for-1202 (solutions/computer-memory)) 0 true])]
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
