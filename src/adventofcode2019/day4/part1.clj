(ns adventofcode2019.day4.part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

; this is the input as it was given, here as a string
(def range_input "356261-846303")
; now, in a better form
(def input_start 356261)
(def input_end 846303)

(defn two_adjacent_digits_are_same [[d1 d2 & rest]]
  (if (= d2 nil)
    false
    (if (= d1 d2)
      true
      (two_adjacent_digits_are_same (cons d2 rest)))))

(defn digits_increase? [[d1 d2 & rest]]
  (if (or (= d2 nil) (and (<= d1 d2) (= rest nil)))
    true
    (if (> d1 d2)
      false
      (digits_increase? (cons d2 rest)))))

(defn digits [number]
  (map #(Character/digit % 10) (str number)))

(defn digits_increase_or_stay_the_same [password]
  (digits_increase? password))

(defn valid_password? [password]
  (let [digits (digits password)]
    (and (two_adjacent_digits_are_same digits) (digits_increase_or_stay_the_same digits))))

(defn -main []
  (let [valid_passwords (filter valid_password? (range input_start (+ 1 input_end)))]
    (println "Valid passwords:" valid_passwords)
    (println "count:" (count valid_passwords))))
