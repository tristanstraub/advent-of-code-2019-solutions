(ns advent.solutions
  (:require [clojure.java.io :as io]))

(defn read-lines
  "Read the lines of a resource"
  [resource]
  (with-open [rdr (io/reader resource)]
    (doall (line-seq rdr))))

(defn mass-to-fuel
  "Calculate the mass required for a module"
  [mass]
  (- (long (/ mass 3)) 2))

(defn module-masses
  "Read module masses from resource day-1.txt"
  []
  (map #(Long/parseLong %) (read-lines (io/resource "data/day-1.txt"))))

(defn day-1
  []
  (reduce + (map mass-to-fuel (module-masses))))

(defn recursive-mass-to-fuel
  "Recursively apply mass-to-fuel to consecutive values of mass-to-fuel"
  [mass]
  (let [fuel-mass (mass-to-fuel mass)]
    (if (pos? fuel-mass)
      (+ fuel-mass (recursive-mass-to-fuel fuel-mass))
      0)))

(defn day-2
  []
  (reduce + (map recursive-mass-to-fuel (module-masses))))



