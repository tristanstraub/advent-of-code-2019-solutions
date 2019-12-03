(ns advent.solutions-test
  (:require [advent.solutions :as solutions]
            [clojure.test :refer [deftest testing is]]))

(deftest day-1-test
  (testing "day-1-part-1"
    (is (= (solutions/day-1-part-1) 3325342)))
  (testing "day-1-part-2"
    (is (= (solutions/day-1-part-2) 4985158))))

(deftest day-2-test
  (testing "part-1"
    (testing "examples"
      (is (= (solutions/interpret [[1 0 0 0 99] 0 true])
             [[2 0 0 0 99] 4 false]))
      (is (= (solutions/interpret [[2 3 0 3 99] 0 true])
             [[2 3 0 6 99] 4 false]))
      (is (= (solutions/interpret [[2 4 4 5 99 0] 0 true])
             [[2 4 4 5 99 9801] 4 false]))
      (is (= (solutions/interpret [[1 1 1 4 99 5 6 0 99] 0 true])
             [[30 1 1 4 2 5 6 0 99] 8 false])))
    (testing "1202 program alarm"
      (is (= (let [[memory _ _] (solutions/interpret [(solutions/set-memory-for-1202 (solutions/computer-memory)) 0 true])]
               (first memory))
             6327510))))
  (testing "part-2"
    (is (= (solutions/matching-inputs (solutions/computer-memory) 19690720)
           [41 12]))
    (is (= (let [[noun verb] (solutions/matching-inputs (solutions/computer-memory) 19690720)]
             (+ verb (* noun 100)))
           4112))))



(deftest day-3-part-1
  (testing "part-1"
    (is (= (solutions/closest-manhattan-to-center [(solutions/parse-wire "R8,U5,L5,D3")
                                                   (solutions/parse-wire "U7,R6,D4,L4")])
           6))

    (is (= (solutions/closest-manhattan-to-center [(solutions/parse-wire "R75,D30,R83,U83,L12,D49,R71,U7,L72")
                                                   (solutions/parse-wire "U62,R66,U55,R34,D71,R55,D58,R83")])
           159))

    (is (= (solutions/closest-manhattan-to-center (map solutions/parse-wire ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                                                             "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))
           135))

    (is (= (solutions/closest-manhattan-to-center (solutions/wires))
           399)))
  (testing "part-2"
    (is (= (solutions/closest-length-to-center (map solutions/parse-wire ["R8,U5,L5,D3" "U7,R6,D4,L4"]))
           30))
    (is (= (solutions/closest-length-to-center (map solutions/parse-wire ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                                                          "U62,R66,U55,R34,D71,R55,D58,R83"]))
           610))

    (is (= (solutions/closest-length-to-center (map solutions/parse-wire ["R75,D30,R83,U83,L12,D49,R71,U7,L72"
                                                                          "U62,R66,U55,R34,D71,R55,D58,R83"]))
           610))

    (is (= (solutions/closest-length-to-center (map solutions/parse-wire ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
                                                                          "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]))
           410))    

    (is (= (solutions/closest-length-to-center (solutions/wires))
           15678))))
