(ns adventofcode2019.day3.part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
    ;[clojure.set :refer [set]]
            [defun.core :refer [defun]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(def input_file "resources/day3.input")

(defn hashmap_value [str_direction]
  (let [direction (keyword (subs str_direction 0 1))
        steps (Integer/parseInt (subs str_direction 1))]
    {direction steps}))

(defn calculate_fields_from_statement [position direction steps]
  (let [x (position :x)
        y (position :y)
        new_fields (match [direction]
                          [:R] (map (fn [x] {:x x, :y y}) (range x (+ 1 x steps)))
                          [:L] (map (fn [x] {:x x, :y y}) (reverse (range (- x steps) (+ 1 x))))
                          [:U] (map (fn [y] {:x x, :y y}) (range y (+ 1 y steps)))
                          [:D] (map (fn [y] {:x x, :y y}) (reverse (range (- y steps) (+ 1 y)))))
        new_position (last new_fields)]
    [(set new_fields) new_position]))

(defn calculate_concrete_fields [statements position fields]
  (match [statements]
         [[]] fields
         [[statement & others]]
         (let [direction (key (first statement))
               steps (val (first statement))
               [new_fields position] (calculate_fields_from_statement position direction steps)
               fields (set/union fields new_fields)
               ]
           ;(calculate_concrete_fields others position (conj fields (str direction)))
           (calculate_concrete_fields others position fields)
           )))

(defn calculate_allocated_fields [wire]
  (let [wire_hashmap (mapv hashmap_value wire)]
    (calculate_concrete_fields wire_hashmap {:x 0 :y 0} #{})))

(defn get_crossings [fields1 fields2]
  (set/intersection fields1 fields2))                       ; TODO: (set/intersection ...)

(defn calculate_manhatten_distance [point1 point2]
  "TODO")

(defn distance [crossing]
  (let [x_distance (math/abs (crossing :x))
        y_distance (math/abs (crossing :y))]
    (+ x_distance y_distance)))

(defn manhatten_distance [wire1 wire2]
  (let [
        allocated_fields1 (calculate_allocated_fields wire1)
        allocated_fields2 (calculate_allocated_fields wire2)
        ;[crossing1 crossing2] (get_crossings allocated_fields1 allocated_fields2)
        crossings (get_crossings allocated_fields1 allocated_fields2)
        ]
    (do
      (println "fields1 =" (sort-by :x (sort-by :y (vec allocated_fields1))))
      (println "fields2 =" (sort-by :x (sort-by :y (vec allocated_fields2))))
      (println "crossings =" crossings)
      (map distance crossings))
    ))

(defn -main []
  (let [[wire1 wire2] (read_csv_as_string_multiple_lines (slurp input_file))]
    (println "wire1 =" wire1)
    (println "wire2 =" wire2)
    (println "manhatten distance =" (sort (manhatten_distance wire1 wire2)))))