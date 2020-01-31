(ns adventofcode2019.day14part2
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:use [adventofcode2019.core]))

(defn to-resource-object [s]
  (let [[amount resource] (str/split s #" ")]
    [(keyword resource) (Integer/parseInt amount)]))

(defn parse-transformation [line]
  (let [[from to] (str/split line #" => ")
        from (str/split from #", ")
        from (into {} (map to-resource-object from))]
    [from (to-resource-object to)]))

(defn parse-input [input]
  (map parse-transformation (str/split-lines input)))


(defn get-transformation-for [[t & transformations] resource]
  (let [t-resource (first (second t))]
    (if (= t-resource resource)
      t
      (recur transformations resource))))


(defn subtract-from-produces [produces produce]
  (merge-with - produces (apply hash-map produce)))

(defn add-to-produces [produces produce]
  (merge-with + produces (apply hash-map produce)))

(defn smallest-multiple-greater-than [multiple x]
  "E.g., if multiple is 5 and x is 13, returns 15."
  (* multiple (first (drop-while (fn [i] (< (* multiple i) x)) (iterate inc 1)))))

(defn get-transformations-for-multiple [transformations free-produces [resource amount]]
  (let [[free-amount free-produces] (if (contains? free-produces resource)
                                      (let [free-amount (free-produces resource)]
                                        [free-amount (subtract-from-produces free-produces [resource free-amount])])
                                      [0 free-produces])

        [from-produces result] (get-transformation-for transformations resource)
        effective-amount (smallest-multiple-greater-than (second result) (- amount free-amount))
        multiplier (/ effective-amount (second result))

        free-produce (assoc result 1 (+ free-amount (- effective-amount amount)))
        free-produces (add-to-produces free-produces free-produce)]
    [(into {} (map (fn [[k v]] [k (* v multiplier)]) from-produces)) free-produces]))

(defn contains-enough? [produces produce-needed]
  (and (contains? produces (first produce-needed)) (> (produces (first produce-needed)) (second produce-needed))))


(defn back-transform-rec [transformations produce-needed free-produces amount-ore]
  (if (empty? produce-needed)
    amount-ore
    (if (contains? produce-needed :ORE)
      (recur transformations (dissoc produce-needed :ORE) free-produces (+ amount-ore (produce-needed :ORE)))
      (let [next-produce (first produce-needed)
            produce-needed (into {} (rest produce-needed))]
        (if (contains-enough? free-produces next-produce)
          (recur transformations produce-needed (subtract-from-produces free-produces next-produce) amount-ore)
          (let [[further-produce-needed free-produces] (get-transformations-for-multiple transformations free-produces next-produce)
                produce-needed (merge-with + produce-needed further-produce-needed)]
            (recur transformations produce-needed free-produces amount-ore)))))))


(defn count-ore-for-fuel [transformations]
  (back-transform-rec transformations {:FUEL 10} {} 0))

; Observations:
; wanted:   1000000000000
; 100000 => 60999050357
; 400000 => 243995587741
; 500000 =>

(defn -main []
  (let [input (slurp "resources/day14.input")

        ; test inputs
        ; this should produce 31 ORE for 1 FUEL
        input "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"

        ; this requires 165 ORE for 1 FUEL
        ;input "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"

        ; result is 13312
        ;input "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

        transformations (parse-input input)]
    (do
      ;(println transformations)
      ;(println (get-transformation-for transformations "FUEL"))

      ;(println 1000000000000)
      (println (count-ore-for-fuel transformations))

      )))

; Incorrect:
; 1161027 too high