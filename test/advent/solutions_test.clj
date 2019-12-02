(ns advent.solutions-test
  (:require [advent.solutions :as solutions]
            [clojure.test :refer [deftest testing is]]))

(deftest day-1-test
  (testing "day-1-part-1"
    (is (= (solutions/day-1-part-1) 3325342)))
  (testing "day-1-part-2"
    (is (= (solutions/day-1-part-2) 4985158))))

(deftest day-2-test
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

