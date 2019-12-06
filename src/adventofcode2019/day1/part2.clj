(ns adventofcode2019.day1.part2
  (:require [clojure.string :as str]))

(def input "resources/day1.input")

(defn fuel_requirement_single [mass]
  (- (int (/ mass 3)) 2))

(defn fuel_requirement_rec [mass_so_far]
  (let [new_fuel (fuel_requirement_single mass_so_far)]
    (if (< new_fuel 0) 0 (+ new_fuel (fuel_requirement_rec new_fuel)))))

(defn calculate_total [input_file_path]
  (let [input (slurp input_file_path)
        input_list (str/split-lines input)
        input_as_ints (mapv #(Integer/parseInt %) input_list)
        fuel_requirements (mapv fuel_requirement_rec input_as_ints)]
    (reduce + fuel_requirements)))

(defn -main []
  (println (calculate_total input)))