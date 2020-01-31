(ns adventofcode2019.day14part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:use [adventofcode2019.core]))

(defn to-resource-object [s]
  (let [[amount resource] (str/split s #" ")]
    {:resource resource :amount (Integer/parseInt amount)}))

(defn parse-transformation [line]
  (let [[from to] (str/split line #" => ")
        from (str/split from #", ")
        from (map to-resource-object from)]
    [from (to-resource-object to)]))

(defn parse-input [input]
  (map parse-transformation (str/split-lines input)))

(defn get-transformation-for [[t & transformations] resource]
  (let [t-resource ((second t) :resource)]
    (if (= t-resource resource)
      t
      (recur transformations resource))))

(defn contains-kv? [[c & cs] k v]
  (if (nil? c)
    false
    (if (= (c k) v)
      true
      (recur cs k v))))

(defn get-produce [[p & produces] resource]
  (if (nil? p)
    nil
    (if (= (p :resource) resource)
      p
      (recur produces resource))))


(defn op-with-produce-amount [produces produce op]
  (vec (map (fn [p] (if (= (p :resource) (produce :resource))
                      {:resource (p :resource) :amount (op (p :amount) (produce :amount))}
                      p))
            produces)))

(defn remove-from-produces [produces produce]
  (op-with-produce-amount produces produce -))

(defn add-to-produces [produces produce]
  (if (contains-kv? produces :resource (produce :resource))
    (op-with-produce-amount produces produce +)
    (conj produces produce)))

(defn smallest-multiple-greater-than [multiple x]
  "E.g., if multiple is 5 and x is 13, returns 15."
  (* multiple (first (drop-while (fn [i] (< (* multiple i) x)) (iterate inc 1)))))

(defn get-transformations-for-multiple [transformations free-produces {:keys [resource amount]}]
  (let [[free-amount free-produces] (if-let [free-produce (get-produce free-produces resource)]
                                      [(free-produce :amount) (remove-from-produces free-produces free-produce)]
                                      [0 free-produces])

        [from-produces result] (get-transformation-for transformations resource)
        effective-amount (smallest-multiple-greater-than (result :amount) (- amount free-amount))
        multiplier (/ effective-amount (result :amount))

        free-produce (assoc result :amount (+ free-amount (- effective-amount amount)))
        free-produces (add-to-produces free-produces free-produce)]
    [(map #(update-in % [:amount] * multiplier) from-produces) free-produces]))

(defn contains-enough? [[p & produces] produce-needed]
  (if (nil? p)
    false
    (if (and (= (p :resource) (produce-needed :resource))
             (>= (p :amount) (produce-needed :amount)))
      true
      (recur produces produce-needed))))


(defn back-transform-rec [transformations [pn & produce-needed] free-produces amount-ore]
  (if (nil? pn)
    amount-ore
    (if (= (pn :resource) "ORE")
      (recur transformations produce-needed free-produces (+ amount-ore (pn :amount)))
      (if (contains-enough? free-produces pn)
        (recur transformations produce-needed (remove-from-produces free-produces pn) amount-ore)
        (let [[further-produce-needed free-produces] (get-transformations-for-multiple transformations free-produces pn)
              produce-needed (apply conj produce-needed further-produce-needed)]
          (recur transformations produce-needed free-produces amount-ore))))))


(defn count-ore-for-fuel [transformations]
  (let [produce-needed (first (get-transformation-for transformations "FUEL"))]
    (back-transform-rec transformations produce-needed [] 0)))


(defn -main []
  (let [input (slurp "resources/day14.input")

        ; test inputs
        ; this should produce 31 ORE for 1 FUEL
        ;input "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"

        ; this requires 165 ORE for 1 FUEL
        ;input "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"

        ; result is 13312
        ;input "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

        transformations (parse-input input)]
    (do
      ;(println transformations)
      ;(println (get-transformation-for transformations "FUEL"))

      (println (count-ore-for-fuel transformations))

      )))

; Incorrect:
; 1161027 too high