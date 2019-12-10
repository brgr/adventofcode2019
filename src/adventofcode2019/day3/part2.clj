(ns adventofcode2019.day3.part2
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
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
  (set/intersection fields1 fields2))

(defn distance [crossing]
  (let [x_distance (math/abs (crossing :x))
        y_distance (math/abs (crossing :y))]
    (+ x_distance y_distance)))

(defn crossings [wire1 wire2]
  (let [allocated_fields1 (calculate_allocated_fields wire1)
        allocated_fields2 (calculate_allocated_fields wire2)
        crossings (get_crossings allocated_fields1 allocated_fields2)]
    crossings))

(defn calculate_next_pos [wire {:keys [x y]}]
  (do
    ;(println wire)
    (let [direction (key (first wire))
          steps (val (first wire))]
      (match [direction]
             [:R] [(map (fn [x] {:x x, :y y}) (range x (+ 1 x steps))) steps]
             [:L] [(map (fn [x] {:x x, :y y}) (reverse (range (- x steps) (+ 1 x)))) steps]
             [:U] [(map (fn [y] {:x x, :y y}) (range y (+ 1 y steps))) steps]
             [:D] [(map (fn [y] {:x x, :y y}) (reverse (range (- y steps) (+ 1 y)))) steps]
             ))))

(defn steps_between [pos1 pos2]
  (do

    (let [{x1 :x y1 :y} pos1
          {x2 :x y2 :y} pos2]
      (do
        (println "y1 =" y1 "y2 =" y2)
        (if (= x1 x2)
        (math/abs
          (- y1 y2))
        (math/abs (- x1 x2)))))))

(defn calculate_steps_rec [steps [wire & rest] current_pos goal_pos]
  (let [[next_positions next_steps] (calculate_next_pos wire current_pos)]
    (do
      ;(println "goal pos =" goal_pos "/// next positions =" (vec next_positions))
      ;(println "--> contains?" (some #(= goal_pos %) (vec next_positions)))
      (if (some #(= goal_pos %) (vec next_positions))
        (+ steps (steps_between current_pos goal_pos))
        (calculate_steps_rec (+ steps next_steps) rest (last next_positions) goal_pos)))))

(defn calculate_steps_to_point [wire point]
  (do
    ;(println point)
    (calculate_steps_rec 0 wire {:x 0 :y 0} point)))

(defn calculate_total_steps_to_crossing [wire1 wire2 crossing]
  (let [wire1 (mapv hashmap_value wire1)
        wire2 (mapv hashmap_value wire2)
        steps_from_wire1 (calculate_steps_to_point wire1 crossing)
        steps_from_wire2 (calculate_steps_to_point wire2 crossing)]
    (+ steps_from_wire1 steps_from_wire2)
    ))

(defn -main []
  (let [[wire1 wire2] (read_csv_as_string_multiple_lines (slurp input_file))
        crossings (disj (crossings wire1 wire2) {:x 0 :y 0})
        steps (sort (map (partial calculate_total_steps_to_crossing wire1 wire2) crossings))]
    (println "wire1 =" wire1)
    (println "wire2 =" wire2)
    (println "manhatten distance =" crossings)
    (println "total steps =" steps)))