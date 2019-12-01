(ns advent.solutions-test
  (:require [advent.solutions :as solutions]
            [clojure.test :refer [deftest testing is]]))

(deftest day-1-test
  (is (= (solutions/day-1) 3325342)))

(deftest day-2-test
  (is (= (solutions/day-2) 4985158)))
