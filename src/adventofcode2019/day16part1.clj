(ns adventofcode2019.day16part1
  (:require [clojure.core.match :refer [match]]
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
  (do
    (println numbers)
    (for [each-repeat-n (range 1 (+ 1 (count numbers)))]
      (let [base-pattern (base-pattern-each-repeated-shifted (count numbers) each-repeat-n)]
        (mod (Math/abs ^Integer (reduce + (map * numbers base-pattern))) 10)))))


(def input (slurp "resources/day16.input"))
; test input 1
;input "12345678"
; test input 2
;input "80871224585914546619083218645595"
; test input 3
; input "03036732577212944063491565474664"

(def numbers (map (fn [i] (Integer/parseInt (str i))) input))
(transform-numbers numbers)
(take 10 (nth (iterate transform-numbers numbers) 100))

; the original transformation:
(println (transform-numbers numbers))

(defn transform-numbers-quickly [numbers]
  "This transforms the numbers in a much more efficient way than the original approach, by
  taking into account that the second half of the numbers follows an easy pattern to be solved.
  NOTE! This is only meant to be executed on the second half of the numbers. If executed on all
        numbers, the first half will give wrong results!"
  (loop [new-numbers (conj '() (last numbers))
         numbers (rest (reverse numbers))]
    (if (empty? numbers)
      new-numbers
      (let [new-number (mod (+ (first new-numbers) (first numbers)) 10)
            new-numbers (conj new-numbers new-number)]
        (recur new-numbers (rest numbers))))))

(println (transform-numbers-quickly numbers))

(def message-offset
  (Integer/parseInt (reduce str (take 7 numbers))))

(def part2-numbers
  (take (* (count numbers) 10000) (cycle numbers)))

(def count-part2-numbers
  (count part2-numbers))

(def part2-numbers-second-half
  (drop (/ count-part2-numbers 2) part2-numbers))

(def message-offset-for-second-half
  (- message-offset (/ count-part2-numbers 2)))

(def part2-numbers-starting-at-message-offset
  (drop message-offset-for-second-half part2-numbers-second-half))

(def part2-starting-at-message-offset
  (-> (iterate transform-numbers-quickly part2-numbers-starting-at-message-offset)
      (nth 100)))

(def final-result-part2
  (apply str (take 8 part2-starting-at-message-offset)))



(defn -main []
  (let [input (slurp "resources/day16.input")

        ; test input 1
        ;input "12345678"

        ; test input 2
        ;input "80871224585914546619083218645595"
        ; test input 3
        input "03036732577212944063491565474664"

        numbers (map (fn [i] (Integer/parseInt (str i))) input)
        ; TODO: this is too slow!
        numbers (take (* (count numbers) 10000) (cycle numbers))
        ]
    (do
      (println numbers)

      ;(println (repeat-each base-pattern 2))
      ;(println (take 100 (rest (cycle (repeat-each base-pattern 2)))))
      (println (nth (iterate transform-numbers numbers) 100))

      )))
