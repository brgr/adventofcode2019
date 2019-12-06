(ns adventofcode2019.day2.part1
  (:require [clojure.string :as str]))

(def input "resources/day2.input")

(def intcode (mapv #(Integer/parseInt %) (str/split (slurp input) #"\,")))

(defn process_single [intcode position]
  (let [opcode (get intcode position)
        first_element (get intcode (get intcode (+ position 1)))
        second_element (get intcode (get intcode (+ position 2)))
        resulting_position (get intcode (+ position 3))]
    (if (== opcode 1)
      (assoc intcode resulting_position (+ first_element second_element))
      (if (== opcode 2)
        (assoc intcode resulting_position (* first_element second_element))))))

(defn process_intcode [intcode position]
  (if (not= 99 (get intcode position))
   (let [intcode (process_single intcode position)]
    (process_intcode intcode (+ position 4)))
   intcode))

(defn restore_intcode [intcode]
  (assoc (assoc intcode 2 2) 1 12))

(defn -main []
  (println "original: " intcode)
  (println "restored: " (restore_intcode intcode))
  (println "processed: " (process_intcode (restore_intcode intcode) 0)))