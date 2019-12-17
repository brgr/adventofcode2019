(ns adventofcode2019.day7part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [clojure.java.io :as io])
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
      (case mode
        :position (get intcode parameter_value)
        :immediate parameter_value
        ))))

(defn process_single [intcode position instruction [next & rest :as input] output]
  (let [first_parameter (parameter intcode instruction position 1)
        second_parameter (parameter intcode instruction position 2)
        ; All our opcodes so far only (if) ever see the 3rd parameter as the address (position) of the result
        third_parameter (get intcode (+ position 3))]
    (case (instruction :opcode)
      1 [(+ position 4) (assoc intcode third_parameter (+ first_parameter second_parameter)) input output]
      2 [(+ position 4) (assoc intcode third_parameter (* first_parameter second_parameter)) input output]
      ; fixme: the following should probably check that an integer is being read...
      ; Also, it seems that '3'/input always takes the first parameter as an address
      3 [(+ position 2) (assoc intcode (get intcode (+ position 1)) next) rest output]
      4 [(+ position 2) intcode input (conj output first_parameter)]

      5 [(if (= first_parameter 0) (+ position 3) second_parameter) intcode input output]
      6 [(if (not= first_parameter 0) (+ position 3) second_parameter) intcode input output]
      7 [(+ position 4) (if (< first_parameter second_parameter)
                          (assoc intcode third_parameter 1)
                          (assoc intcode third_parameter 0))
         input output]
      8 [(+ position 4) (if (= first_parameter second_parameter)
                          (assoc intcode third_parameter 1)
                          (assoc intcode third_parameter 0))
         input output]
      )))

(defn destructure_instruction [instruction]
  (do
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

(defn process_intcode [intcode position input output]
  (let [instruction (get intcode position)
        instruction (destructure_instruction instruction)]
    (if (= stop_instruction (instruction :opcode))
      output
      (let [[position intcode input output] (process_single intcode position instruction input output)]
        (process_intcode intcode position input output)))))

(defn parse_intcode [input]
  (mapv #(Integer/parseInt %) (str/split input #"\,")))

(defn find_output_signal_rec [intcode [phase_setting & rest] input_signal]
  (if (nil? phase_setting)
    input_signal
    (let [input_signal (first (process_intcode intcode 0 [phase_setting input_signal] []))]
      (find_output_signal_rec intcode rest input_signal))))

(defn find_output_signal [intcode phase_settings]
  (do
    (println phase_settings)
    (find_output_signal_rec intcode phase_settings 0)))

(defn find_largest_output_signal [intcode]
  (let [phase_settings (for [i (range 5) j (range 5) k (range 5) l (range 5) m (range 5)
                             :when (= 5 (count (set [i j k l m])))]
                         [i j k l m])
        results (map (fn [signal] [signal (find_output_signal intcode signal)]) phase_settings)
        max_output (apply max (map #(nth % 1) results))]
    ;(filter (fn [[x y]] (= y max_output)) results)
    max_output
    ))

(defn -main []
  (let [input (slurp "resources/day7.input")

        ; the following, with 43210 given, returns 43210
        ; it is entered as follows: 4 0 3 x 2 x 1 x 0 x, where x is the result
        ; that followed the last inputs so far
        ;input "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"

        ; given: [0 1 2 3 4], returns: 54321
        ;input "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"

        ; given: [1 0 4 3 2], returns: 65210
        ;input "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"

        intcode (parse_intcode input)]
    (do
      (println intcode)
      ;(println (find_output_signal intcode [4 3 2 1 0]))
      ;(println (find_output_signal intcode [0 1 2 3 4]))
      ;(println (find_output_signal intcode [1 0 4 3 2]))
      (println (find_largest_output_signal intcode))
      )))