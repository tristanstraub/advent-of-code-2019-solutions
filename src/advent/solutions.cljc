(ns advent.solutions
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn day-1-part-1
  []
  (reduce + (map mass-to-fuel (module-masses))))

(defn recursive-mass-to-fuel
  "Recursively apply mass-to-fuel to consecutive values of mass-to-fuel"
  [mass]
  (let [fuel-mass (mass-to-fuel mass)]
    (if (pos? fuel-mass)
      (+ fuel-mass (recursive-mass-to-fuel fuel-mass))
      0)))

(defn day-1-part-2
  []
  (reduce + (map recursive-mass-to-fuel (module-masses))))

(defn output
  [state v])

(defn input
  [state])

(defn machine
  [[memory ptr running?]]
  {:memory   memory
   :ptr      ptr
   :running? running?})

(defn interpret-1
  [state]
  (let [{:keys [memory ptr running?]} state]
    (if running?
      (case (nth memory ptr)
        ;; add
        1 (let [[_ src-a src-b dest] (subvec memory ptr (+ ptr 4))]
            (machine [(assoc memory dest (+ (nth memory src-a) (nth memory src-b)))
                      (+ ptr 4)
                      true]))
        ;; mul
        2 (let [[_ src-a src-b dest] (subvec memory ptr (+ ptr 4))]
            (machine [(assoc memory dest (* (nth memory src-a) (nth memory src-b)))
                      (+ ptr 4)
                      true]))
        ;; input
        ;; 3 (let [[_ dest] (subvec memory ptr (+ ptr 2))]
        ;;     [(assoc memory dest (input state))
        ;;      (+ ptr 2)
        ;;      true])
        ;; 4 (let [[_ dest] (subvec memory ptr (+ ptr 2))]
        ;;     (output state (get memory dest))
        ;;     [memory
        ;;      (+ ptr 2)
        ;;      true])
        99 (machine [memory ptr false]))
      state)))

(defn interpret
  [state]
  (loop [state state]
    (let [{:keys [running?]} state]
      (if running?
        (recur (interpret-1 state))
        state))))

(defn computer-memory
  "Read computer memory from resource day-2.txt"
  []
  (mapv #(Long/parseLong %) (str/split (str/trim (slurp (io/resource "data/day-2.txt"))) #",")))

(defn set-inputs
  [memory a b]
  (assoc memory 1 a 2 b))

(defn set-memory-for-1202
  [memory]
  (set-inputs memory 12 2))

(defn interpret-all-inputs
  [memory]
  (for [i (range 0 100)
        j (range 0 100)]
    [(nth (:memory (interpret (machine [(set-inputs memory i j) 0 true]))) 0)
     i
     j]))

(defn matching-inputs
  [memory expected-output]
  (let [[_ input-a input-b] (first (filter #(= (first %) expected-output) (interpret-all-inputs memory)))]
    [input-a input-b]))

(defn parse-point
  [point]
  (let [[a & b] point
        a       (str a)
        b       (apply str b)]
    [(keyword (str/lower-case a))
     (Long/parseLong b)]))

(defn parse-wire
  [wire]
  (map parse-point (str/split wire #",")))

(defn wires
  []
  (map parse-wire (read-lines (io/resource "data/day-3.txt"))))

(defn trace-wire
  [grid wire-id wire]
  (:grid (reduce (fn [{grid        :grid
                       pos         :pos}
                      [direction distance]]
                   (let [ds          (case direction
                                       :l [-1 0]
                                       :r [1 0]
                                       :u [0 -1]
                                       :d [0 1])
                         wire-length (get-in grid [pos wire-id] 0)]
                     (reduce (fn [{grid :grid pos :pos} wire-length]
                               (let [new-pos (mapv + pos ds)]
                                 {:grid (update-in grid [new-pos wire-id]
                                                   (fn [existing-wire-length]
                                                     (or existing-wire-length wire-length)))
                                  :pos  new-pos}))
                             {:grid        grid
                              :pos         pos}
                             (range (inc wire-length) (inc (+ wire-length distance))))))
                 {:grid grid :pos [0 0]}
                 wire)))

(defn traces
  [wires]
  (let [grid {}]
    (reduce (fn [grid [i wire]]
              (trace-wire grid i wire))
            grid
            (map vector (range) wires))))

(defn crossing?
  [[position wire-lengths]]
  (<= 2 (count (keys wire-lengths))))

(defn closest-manhattan-to-center*
  [wires]
  (->> (traces wires)
       (filter crossing?)))

(defn closest-manhattan-to-center
  [wires]
  (->> (traces wires)
       (filter crossing?)
       (map first)
       (map #(reduce + (map (fn [v] (Math/abs v)) %)))
       sort
       first))


(defn closest-length-to-center
  [wires]
  (->> (traces wires)
       (filter crossing?)
       (map (fn [[pos crossing]]
              (reduce + (vals crossing))))
       sort
       first))

(defn password-part1?
  [p a b]
  (and (some (fn [[a b]]
               (= a b))
             (partition 2 1 (str p)))
       (= 6 (count (str p)))
       (<= a p b)
       (every? (fn [[a b]] (<= (read-string (str a)) (read-string (str b))))
               (partition 2 1 (repeat 9) (str p)))))

(defn password-part2?
  [p a b]
  (and (password-part1? p a b)
       (some (fn [[a b c d]]
               (and (= b c)
                    (not= a b)
                    (not= c d)))
             (partition 4 1 (str "x" p "x")))))

(defn passwords-part1
  [a b]
  (for [i (range a b)
        :when (password-part1? i a b)]
    i))

(defn passwords-part2
  [a b]
  (for [i (range a b)
        :when (password-part2? i a b)]
    i))
