(ns adventofcode2019.day1.part1
  (:require [clojure.string :as str]))

(def input "resources/day1.input")

(defn fuel_requirement [mass]
  (- (int (/ (Integer/parseInt mass) 3)) 2)
  ;(- (int (/ (Integer/getInteger mass) 3)) 2)
  )

(defn calculate_total [input_file_path]
  (let [input (slurp input_file_path)
        input_list (str/split-lines input)
        fuel_requirements (mapv fuel_requirement input_list)]
    (reduce + fuel_requirements) ))


(defn -main []
  (println (calculate_total input)))