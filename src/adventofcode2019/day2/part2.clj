(ns adventofcode2019.day2.part2
  (:require [clojure.string :as str]))

(def input "resources/day2.input")
(def expected_output 19690720)
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

(defn change_input [intcode noun verb]
  (assoc (assoc intcode 2 verb) 1 noun))

(def find_noun_and_verb
  (for [noun (range 0 99) verb (range 0 99)]
    (let [processed_intcode (process_intcode (change_input intcode noun verb) 0)]
      (if (== expected_output (get processed_intcode 0)) [noun verb]))))

(defn -main []
  (let [[noun verb] (first (filter (partial not= nil) find_noun_and_verb))]
    (do
      (println "noun =" noun "verb =" verb)
      (println "100*noun+verb =" (+ verb (* 100 noun)))))
  )


