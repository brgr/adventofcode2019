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


(defn count-ore-for-fuel [transformations amount]
  (back-transform-rec transformations [{:resource "FUEL" :amount amount}] [] 0))


(defn -main []
  (let [input (slurp "resources/day14.input")

        ; test inputs
        ; this should produce 31 ORE for 1 FUEL
        ;input "10 ORE => 10 A\n1 ORE => 1 B\n7 A, 1 B => 1 C\n7 A, 1 C => 1 D\n7 A, 1 D => 1 E\n7 A, 1 E => 1 FUEL"

        ; this requires 165 ORE for 1 FUEL
        ;input "9 ORE => 2 A\n8 ORE => 3 B\n7 ORE => 5 C\n3 A, 4 B => 1 AB\n5 B, 7 C => 1 BC\n4 C, 1 A => 1 CA\n2 AB, 3 BC, 4 CA => 1 FUEL"

        ; result is 13312
        ;input "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"

        ; result is 180697
        ;input "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF"

        ; result is 2210736
        ;input "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX"

        transformations (parse-input input)]
    (do
      ;(println transformations)
      ;(println (get-transformation-for transformations "FUEL"))

      ;(println (count-ore-for-fuel transformations 1))
      (println (count-ore-for-fuel transformations 955854)) ; = 583060060057

      )))

; Incorrect:
; 1161027 too high