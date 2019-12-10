(ns adventofcode2019.day5.part2
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(def stop_instruction 99)

; parameter_position starting at 0 for first parameter
(defn parameter_mode [instruction parameter_position]
  (nth (instruction :param_modes) parameter_position))

; parameter_position in relation to opcode_position
(defn parameter [intcode instruction opcode_position parameter_position]
  (let [mode (parameter_mode instruction (- parameter_position 1))
        parameter_value (get intcode (+ opcode_position parameter_position))]
    (do
      ;(println "     intcode:" intcode)
      ;(println "-> parameter value:" parameter_value "/ mode:" mode)
      (case mode
        :position (get intcode parameter_value)
        :immediate parameter_value
        ))))

(defn process_single [intcode position instruction]
  (let [first_parameter (parameter intcode instruction position 1)
        second_parameter (parameter intcode instruction position 2)
        ; All our opcodes so far only (if) ever see the 3rd parameter as the address (position) of the result
        third_parameter (get intcode (+ position 3))]
    (do
      ;(println "-> position:" position)
      ;(println "-> 1st param:" first_parameter)
      ;(println "-> 2nd param:" second_parameter)
      (case (instruction :opcode)
        1 [(+ position 4) (assoc intcode third_parameter (+ first_parameter second_parameter))]
        2 [(+ position 4) (assoc intcode third_parameter (* first_parameter second_parameter))]
        ; fixme: the following should probably check that an integer is being read...
        ; Also, it seems that '3'/input always takes the first parameter as an address
        3 [(+ position 2) (assoc intcode (get intcode (+ position 1)) (read))]
        ; fixme: change to print (without newline)
        4 [(+ position 2) (do (println first_parameter) intcode)]
        5 [(if (= first_parameter 0) (+ position 3) second_parameter) intcode]
        6 [(if (not= first_parameter 0) (+ position 3) second_parameter) intcode]
        7 [(+ position 4) (if (< first_parameter second_parameter)
                            (assoc intcode third_parameter 1)
                            (assoc intcode third_parameter 0))]
        8 [(+ position 4) (if (= first_parameter second_parameter)
                            (assoc intcode third_parameter 1)
                            (assoc intcode third_parameter 0))]
        ))))

(defn destructure_instruction [instruction]
  (do
    ;(println "instruction:" instruction)
    (let [opcode (mod instruction 100)
          transform_param_mode (fn [mode] (if (= mode 1) :immediate :position))
          param1_mode (transform_param_mode (quot (mod instruction 1000) 100))
          param2_mode (transform_param_mode (quot (mod instruction 10000) 1000))
          ; the 3rd param is always position mode, so far
          ;param3_mode (transform_param_mode (quot (mod instruction 100000) 10000))
          ]
      {:opcode      opcode
       :param_modes [param1_mode param2_mode
                     ;:param3 param3_mode
                     ]})))

(defn process_intcode [intcode position]
  (let [instruction (get intcode position)
        instruction (destructure_instruction instruction)]
    (if (= stop_instruction (instruction :opcode))
      intcode
      (let [[position intcode] (process_single intcode position instruction)]
        (process_intcode intcode position)))))

(defn -main []
  (let [input_file "resources/day5.input"
        input (slurp input_file)
        ; output 0 if input was 0, 1 otherwise
        ;input "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
        intcode (mapv #(Integer/parseInt %) (str/split input #"\,"))]
    (do
      (println "Executing intcode:" intcode)
      (println "Processing...")
      (process_intcode intcode 0))))