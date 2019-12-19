(ns adventofcode2019.day9part1
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
    (case mode
      :position (get intcode parameter_value)
      :immediate parameter_value
      )))

(defn destructure_instruction [instruction]
  (let [opcode (mod instruction 100)
        transform_param_mode (fn [mode] (if (= mode 1) :immediate :position))
        param1_mode (transform_param_mode (quot (mod instruction 1000) 100))
        param2_mode (transform_param_mode (quot (mod instruction 10000) 1000))
        ; the 3rd param is always position mode, so far
        ;param3_mode (transform_param_mode (quot (mod instruction 100000) 10000))
        ]
    {:opcode      opcode
     :param_modes [param1_mode param2_mode]}))

(defn process_single [{:keys [intcode position input output]}]
  (let [instruction (destructure_instruction (get intcode position))
        first_parameter (parameter intcode instruction position 1)
        second_parameter (parameter intcode instruction position 2)
        ; All our opcodes so far only (if) ever see the 3rd parameter as the address (position) of the result
        third_parameter (get intcode (+ position 3))]

    (case (instruction :opcode)
      1 {:position (+ position 4)
         :intcode  (assoc intcode third_parameter (+ first_parameter second_parameter))
         :input    input
         :output   output
         :state    :running}
      2 {:position (+ position 4)
         :intcode  (assoc intcode third_parameter (* first_parameter second_parameter))
         :input    input
         :output   output
         :state    :running}

      ; 3 -- Input -- Write the next given input in (the *address* specified in) the 1st parameter
      ; fixme: the following should probably check that an integer is being read?
      ; Also, it seems that '3'/input *always* takes the first parameter as an address
      3 {:position (+ position 2)
         :intcode  (assoc intcode (get intcode (+ position 1)) (first input))
         :input    (rest input)
         :output   output
         :state    :running}

      ; 4 -- Output --
      4 {:position (+ position 2)
         :intcode  intcode
         :input    input
         :output   (conj output first_parameter)
         :state    :halting}                                ; Note: In D7P2, output triggers a halting machine!

      5 {:position (if (= first_parameter 0) (+ position 3) second_parameter)
         :intcode  intcode
         :input    input
         :output   output
         :state    :running}
      6 {:position (if (not= first_parameter 0) (+ position 3) second_parameter)
         :intcode  intcode
         :input    input
         :output   output
         :state    :running}
      7 {:position (+ position 4)
         :intcode  (if (< first_parameter second_parameter)
                     (assoc intcode third_parameter 1)
                     (assoc intcode third_parameter 0))
         :input    input
         :output   output
         :state    :running}
      8 {:position (+ position 4)
         :intcode  (if (= first_parameter second_parameter)
                     (assoc intcode third_parameter 1)
                     (assoc intcode third_parameter 0))
         :input    input
         :output   output
         :state    :running}
      )))

(defn process_machine [{:keys [position intcode] :as machine}]
  (if (= stop_instruction (get intcode position))
    (assoc machine :state :stopped)
    (let [machine (process_single machine)]
      (if (= (machine :state) :halting)
        machine
        (process_machine machine)))))

(defn remove_last_output [{:keys [output] :as machine}]
  (let [single_output (last output)
        output (butlast output)
        machine (assoc machine :output output)]
    [machine single_output]))

(defn add_to_input [{:keys [input] :as machine} new_single_input]
  (let [input (conj input new_single_input)]
    (assoc machine :input input)))


(defn init_machine [intcode]
  {:position 0 :intcode intcode :input [] :output [] :state :running})




(defn parse_intcode [input]
  (mapv #(Integer/parseInt %) (str/split input #"\,")))

(defn -main []
  (let [input (slurp "resources/day9.input")

        intcode (parse_intcode input)]
    (do
      (println intcode)
      )))