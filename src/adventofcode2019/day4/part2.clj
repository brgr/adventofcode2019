(ns adventofcode2019.day4.part2
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

(defn contains_one_pair_where_only_two_adjacent_rec [[d1 d2 d3 d4 & rest]]
  (let [only_middle_two_are_equal (and (= d2 d3) (not= d1 d2) (not= d3 d4))
        only_last_two_are_equal (and (= d3 d4) (not= d2 d3))]
    (if (= rest nil)
      (or only_middle_two_are_equal only_last_two_are_equal)
      (if only_middle_two_are_equal
        true~
        (contains_one_pair_where_only_two_adjacent_rec (cons d2 (cons d3 (cons d4 rest))))))))
~
(defn contains_one_pair_where_only_two_adjacent [[d1 d2 d3 & rest]]
  (let [only_first_two_are_equal (and (= d1 d2) (not= d2 d3))]
    ; Attention! The first two may only be checked for the whole password, not for every subset of the password
    ; later on as well!
    (if only_first_two_are_equal
      true
      (contains_one_pair_where_only_two_adjacent_rec (cons d1 (cons d2 (cons d3 rest)))))))

(defn valid_password? [password]
  (let [digits (digits password)]
    (and
      (two_adjacent_digits_are_same digits)
      (digits_increase_or_stay_the_same digits)
      (contains_one_pair_where_only_two_adjacent digits))))

; From (wrong) results I know the number has to be:
; 176 < x < 391
; also: not [274 363]
(defn -main []
  (let [valid_passwords (filter valid_password? (range input_start (+ 1 input_end)))]
    (println "Valid passwords:" valid_passwords)
    (println "count:" (count valid_passwords))))
