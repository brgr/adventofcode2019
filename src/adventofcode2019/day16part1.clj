(ns adventofcode2019.day16part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [intcode])
  (:use [adventofcode2019.core]))

(def base-pattern '(0 1 0 -1))

(defn repeat-each [coll n]
  (apply concat (map #(repeat n %) coll)))

(defn base-pattern-each-repeated-shifted [count each-repeat-n]
  (take count (rest (cycle (repeat-each base-pattern each-repeat-n)))))

; TODO: in 1 round, the n-th element is calculated like this:
;  for i < length(numbers):
;    numbers[i] * base-pattern-each-repeated-shifted(length(numbers), n)[i]

(defn transform-numbers [numbers]
  (for [each-repeat-n (range 1 (+ 1 (count numbers)))]
    (let [base-pattern (base-pattern-each-repeated-shifted (count numbers) each-repeat-n)]
      (mod (Math/abs ^Integer (reduce + (map * numbers base-pattern))) 10))))


(defn -main []
  (let [input (slurp "resources/day16.input")

        ; test input 1
        ;input "12345678"

        ; test input 2
        ;input "80871224585914546619083218645595"

        numbers (map (fn [i] (Integer/parseInt (str i))) input)]
    (do
      (println numbers)

      ;(println (repeat-each base-pattern 2))
      ;(println (take 100 (rest (cycle (repeat-each base-pattern 2)))))
      (println (str/join (take 8 (nth (iterate transform-numbers numbers) 100))))

      )))
