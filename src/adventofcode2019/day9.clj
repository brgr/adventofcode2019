(ns adventofcode2019.day9
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [clojure.java.io :as io])
  (:use [adventofcode2019.core]))

(def stop_instruction 99)

;; get intcode, i.e. get, but returns 0 instead of nil
(defn geti [intcode position]
  (let [result (get intcode position)]
    (if (nil? result) 0 result)))

; parameter_position starting at 0 for first parameter
(defn parameter_mode [instruction parameter_position]
  (nth (instruction :param_modes) parameter_position))

; parameter_position in relation to opcode_position
(defn parameter [intcode instruction opcode_position relative-base parameter_position]
  (let [mode (parameter_mode instruction (- parameter_position 1))
        parameter_value (geti intcode (+ opcode_position parameter_position))]
    (case mode
      :position (geti intcode parameter_value)
      :immediate parameter_value
      :relative (geti intcode (+ relative-base parameter_value)))))

;; Note: The third parameter is always the resulting position, either directly taken from the parameter value or
;; together with the relative base. IMO, this is not fully clear from the puzzle descriptions. This is, however, why
;; the 3rd parameter is here processed using a separate function. It could probably be done in a slightly nicer way by
;; processing all parameters just like this and process them further when using them. This would have needed a little
;; more rewriting, however. Hence why it was so far not done this way.
; parameter_position in relation to opcode_position
(defn third_parameter [intcode instruction opcode_position relative-base parameter_position]
  (let [mode (parameter_mode instruction (- parameter_position 1))
        parameter_value (geti intcode (+ opcode_position parameter_position))]
    (case mode
      :position parameter_value
      :relative (+ relative-base parameter_value))))

(defn transform_param_mode [mode]
  (case mode
    0 :position
    1 :immediate
    2 :relative))

(defn destructure_instruction [instruction]
  (let [opcode (mod instruction 100)
        param1_mode (transform_param_mode (quot (mod instruction 1000) 100))
        param2_mode (transform_param_mode (quot (mod instruction 10000) 1000))]
    {:opcode      opcode
     :param_modes [param1_mode param2_mode]}))

(defn process_single [{:keys [intcode position relative-base input output]}]
  (let [instruction (destructure_instruction (geti intcode position))
        first_parameter (parameter intcode instruction position relative-base 1)
        second_parameter (parameter intcode instruction position relative-base 2)
        third_parameter (third_parameter intcode instruction position relative-base 3)]

    (case (instruction :opcode)
      ;; 1 -- ADD -- Add 1st+2nd param and put result to 3rd
      1 {:position (+ position 4)
         :relative-base relative-base
         :intcode (assoc intcode third_parameter (+ first_parameter second_parameter))
         :input    input
         :output   output
         :state    :running}

      ;; 2 -- MUL -- Multiply 1st+2nd param and put result to 3rd
      2 {:position (+ position 4)
         :relative-base relative-base
         :intcode (assoc intcode third_parameter (* first_parameter second_parameter))
         :input    input
         :output   output
         :state    :running}

      ; 3 -- Input -- Write the next given input in (the *address* specified in) the 1st parameter
      ; Also, it seems that '3'/input *always* takes the first parameter as an address
      ; However, in D9 that seems not to be the case anymore, but I don't know how exactly it is meant
      3 {:position (+ position 2)
         :relative-base relative-base
         :intcode  (assoc intcode third_parameter (first input))
         :input    (rest input)
         :output   output
         :state    :running}

      ; 4 -- Output --
      4 {:position (+ position 2)
         :relative-base relative-base
         :intcode  intcode
         :input    input
         :output   (conj output first_parameter)
         :state    :running}

      5 {:position (if (= first_parameter 0) (+ position 3) second_parameter)
         :relative-base relative-base
         :intcode  intcode
         :input    input
         :output   output
         :state    :running}

      ;; 6 -- JUMP IF -- If 1st param = 1, jump to 2nd param
      6 {:position (if (not= first_parameter 0) (+ position 3) second_parameter)
         :relative-base relative-base
         :intcode  intcode
         :input    input
         :output   output
         :state    :running}
      7 {:position (+ position 4)
         :relative-base relative-base
         :intcode  (if (< first_parameter second_parameter)
                     (assoc intcode third_parameter 1)
                     (assoc intcode third_parameter 0))
         :input    input
         :output   output
         :state    :running}

      ;; 8 -- EQUALS -- If (1st = 2nd) , write 1 to 3rd, else 0 to 3rd
      8 {:position (+ position 4)
         :relative-base relative-base
         :intcode  (if (= first_parameter second_parameter)
                     (assoc intcode third_parameter 1)
                     (assoc intcode third_parameter 0))
         :input    input
         :output   output
         :state    :running}

      ; 9 -- Adjust relative base using its only parameter
      9 {:position (+ position 2)
         :relative-base (+ relative-base first_parameter)
         :intcode  intcode
         :input    input
         :output   output
         :state    :running}
      )))

(defn process_machine [{:keys [position intcode] :as machine}]
  (if (= stop_instruction (geti intcode position))
    (assoc machine :state :stopped)
    (let [machine (process_single machine)]
      (if (= (machine :state) :halting)
        machine
        (recur machine)))))

(defn remove_last_output [{:keys [output] :as machine}]
  (let [single_output (last output)
        output (butlast output)
        machine (assoc machine :output output)]
    [machine single_output]))

(defn add_to_input [{:keys [input] :as machine} new_single_input]
  (let [input (conj input new_single_input)]
    (assoc machine :input input)))

(defn init_machine
  ([intcode] {:position 0 :relative-base 0 :intcode intcode :input [] :output [] :state :running})
  ([intcode input] {:position 0 :relative-base 0 :intcode intcode :input input :output [] :state :running}))

(defn parse_intcode [input]
  (let [intcode_vector (mapv #(Long/parseLong %) (str/split input #"\,"))]
    (into {} (map hash-map (take (count intcode_vector) (iterate inc 0)) intcode_vector))))

(defn -main []
  (let [input (slurp "resources/day9.input")

        ;; Test inputs (the machines do not need any input ever for the test inputs)
        ; ✔️ should produce a copy of itself as output
        ;input "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"

        ; ✔️ should output a 16-digit number
        ;input "1102,34915192,34915192,7,4,7,99,0"

        ; ✔️ should output the large number in the middle
        ;input "104,1125899906842624,99"

        intcode (parse_intcode input)
        ; Here, both part 1 and part 2 can be done, without the need to copy the file, as the only change is the input
        ; from 1 to 2
        machine (init_machine intcode [1])
        ;machine (init_machine intcode [2])
        output ((process_machine machine) :output)
        ]
    (do
      (println intcode)
      (println machine)
      (println output)
      )))

; incorrect answers:
; 203
; 2809146684 (too high)