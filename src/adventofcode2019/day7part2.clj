(ns adventofcode2019.day7part2
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

(defn process_amplifiers_rec [machines this_position]
  (let [running_machine (assoc (get machines this_position) :state :running)
        processed_machine (process_machine running_machine)
        machines (assoc machines this_position processed_machine)]
    (case (processed_machine :state)
      :halting (let [next_position (mod (+ this_position 1) 5)
                     [processed_machine last_output] (remove_last_output processed_machine)
                     next_machine (get machines next_position)
                     next_machine (add_to_input next_machine last_output)
                     machines (assoc (assoc machines this_position processed_machine) next_position next_machine)]
                 (process_amplifiers_rec machines next_position))
      :stopped machines)))

(defn process_amplifiers [machines]
  (process_amplifiers_rec machines 0))

(defn init_machine [intcode]
  {:position 0 :intcode intcode :input [] :output [] :state :running})

(defn signal_to_thrusters
  "Starts 5 machines with the given intcode. Each machine is given, as a start, the respective phase setting, which are
  also given in the argument. The first machine is furthermore given a 0 as a second input. Every time a machine
  produces output, it halts (it only halts - it does not stop!). The output is then given to the next machine in line as
  input (If a machine has not been started yet, it is first given its phase setting, then the last output of the last
  machine). As such it is circling through the machines until all machines are stopped (Note: Or until only the
  last/fifth machine is stopped?). The last output is then returned."
  [intcode phase_settings]
  (let [machines (repeat 5 (init_machine intcode))
        machines (vec (map (fn [m, ps] (assoc m :input [ps])) machines phase_settings))
        amplifier_machines (vec (assoc-in machines [0 :input] (vector (first phase_settings) 0)))]
    (last ((last (process_amplifiers amplifier_machines)) :output))))

(defn find_largest_output_signal [intcode]
  (let [phase_settings (for [i (range 5 10) j (range 5 10) k (range 5 10) l (range 5 10) m (range 5 10)
                             :when (= 5 (count (set [i j k l m])))]
                         [i j k l m])
        results (map (fn [signal] [signal (signal_to_thrusters intcode signal)]) phase_settings)
        max_output (apply max (map #(nth % 1) results))]
    max_output))

(defn parse_intcode [input]
  (mapv #(Integer/parseInt %) (str/split input #"\,")))

(defn -main []
  (let [input (slurp "resources/day7.input")

        ; should give: 139629729 for [9 8 7 6 5]
        ;input "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"

        intcode (parse_intcode input)]
    (do
      (println intcode)
      ;(println (signal_to_thrusters intcode [9 8 7 6 5]))
      (println (find_largest_output_signal intcode))
      )))