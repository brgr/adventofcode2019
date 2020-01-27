(ns intcode
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]])
  (:use [adventofcode2019.core]))

(def stop-instruction 99)

(defn string [{:keys [position relative-base intcode input output state] :as machine}]
  (str "{:position " position
       ", :relative-base " relative-base
       ", input " input
       ", :output " output
       ", :state " state
       ", :intcode" (into (sorted-map) intcode)
       "}"))

;; get intcode, i.e. get, but returns 0 instead of nil
(defn- geti [intcode position]
  (let [result (get intcode position)]
    (if (nil? result) 0 result)))

; parameter_position starting at 0 for first parameter
(defn- parameter-mode [instruction parameter-position]
  (nth (instruction :param-modes) parameter-position))

; parameter_position in relation to opcode_position
(defn- parameter [intcode instruction opcode_position relative-base parameter_position]
  (let [mode (parameter-mode instruction (- parameter_position 1))
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
(defn- third-parameter [intcode instruction opcode-position relative-base parameter-position]
  (let [mode (parameter-mode instruction (- parameter-position 1))
        parameter-value (geti intcode (+ opcode-position parameter-position))]
    (case mode
      :position parameter-value
      :relative (+ relative-base parameter-value))))

(defn- transform-param-mode [mode]
  (case mode
    0 :position
    1 :immediate
    2 :relative))

(defn- destructure-instruction [instruction]
  (let [opcode (mod instruction 100)
        param1-mode (transform-param-mode (quot (mod instruction 1000) 100))
        param2-mode (transform-param-mode (quot (mod instruction 10000) 1000))
        param3-mode (transform-param-mode (quot (mod instruction 100000) 10000))]
    {:opcode      opcode
     :param-modes [param1-mode param2-mode param3-mode]}))

(defn- process-single [{:keys [intcode position relative-base input output] :as machine}]
  (let [instruction (destructure-instruction (geti intcode position))
        first-parameter (parameter intcode instruction position relative-base 1)
        second-parameter (parameter intcode instruction position relative-base 2)
        third-parameter (third-parameter intcode instruction position relative-base 3)]

    (case (instruction :opcode)
      ;; 1 -- ADD -- Add 1st+2nd param and put result to 3rd
      1 {:position      (+ position 4)
         :relative-base relative-base
         :intcode       (assoc intcode third-parameter (+ first-parameter second-parameter))
         :input         input
         :output        output
         :state         :running}

      ;; 2 -- MUL -- Multiply 1st+2nd param and put result to 3rd
      2 {:position      (+ position 4)
         :relative-base relative-base
         :intcode       (assoc intcode third-parameter (* first-parameter second-parameter))
         :input         input
         :output        output
         :state         :running}

      ; 3 -- Input -- Write the next given input in the 1st parameter
      ; Blocks if currently no input is given
      3 (if (empty? input)
          (assoc machine :state :halting)
          {:position      (+ position 2)
           :relative-base relative-base
           ; Fixme: This currently only allows :position mode! If the intcode contains a 203 opcode, i.e. input in
           ;  :relative mode, this needs to be able to handle this (the function would be the same as third-parameter,
           ;  although with the 1st parameter, of course)
           :intcode       (assoc intcode (get intcode (+ position 1)) (first input))
           :input         (rest input)
           :output        output
           :state         :running})

      ; 4 -- Output -- Add the 1st parameter to the output
      4 {:position      (+ position 2)
         :relative-base relative-base
         :intcode       intcode
         :input         input
         :output        (conj output first-parameter)
         :state         :running}

      5 {:position      (if (= first-parameter 0) (+ position 3) second-parameter)
         :relative-base relative-base
         :intcode       intcode
         :input         input
         :output        output
         :state         :running}

      ;; 6 -- JUMP IF -- If 1st param = 1, jump to 2nd param
      6 {:position      (if (not= first-parameter 0) (+ position 3) second-parameter)
         :relative-base relative-base
         :intcode       intcode
         :input         input
         :output        output
         :state         :running}
      7 {:position      (+ position 4)
         :relative-base relative-base
         :intcode       (if (< first-parameter second-parameter)
                          (assoc intcode third-parameter 1)
                          (assoc intcode third-parameter 0))
         :input         input
         :output        output
         :state         :running}

      ;; 8 -- EQUALS -- If (1st = 2nd) , write 1 to 3rd, else 0 to 3rd
      8 {:position      (+ position 4)
         :relative-base relative-base
         :intcode       (if (= first-parameter second-parameter)
                          (assoc intcode third-parameter 1)
                          (assoc intcode third-parameter 0))
         :input         input
         :output        output
         :state         :running}

      ; 9 -- Adjust relative base using its only parameter
      9 {:position      (+ position 2)
         :relative-base (+ relative-base first-parameter)
         :intcode       intcode
         :input         input
         :output        output
         :state         :running}
      )))

(defn run-machine [{:keys [position intcode] :as machine}]
  (if (= stop-instruction (geti intcode position))
    (assoc machine :state :stopped)
    (let [machine (process-single machine)]
      (if (= (machine :state) :halting)
        machine
        (recur machine)))))

(defn remove-last-output [{:keys [output] :as machine}]
  ; Note: Since conj on sequences puts the next element in front, the last output is
  ;  found at the first element
  (let [single-output (last output)
        machine (assoc machine :output (butlast output))]
    [machine single-output]))

(defn conj-input [{:keys [input] :as machine} new-single-input]
  (let [input (conj input new-single-input)]
    (assoc machine :input (seq input))))

(defn init-machine
  ([intcode] {:position 0 :relative-base 0 :intcode intcode :input '() :output '() :state :running})
  ([intcode input] {:position 0 :relative-base 0 :intcode intcode :input input :output [] :state :running}))

(defn parse-input [input]
  (let [intcode-vector (mapv #(Long/parseLong %) (str/split input #"\,"))]
    (into {} (map hash-map (take (count intcode-vector) (iterate inc 0)) intcode-vector))))

(defn -main []
  "Test the intcode runner with the intcode and input given in day 9"
  (let [intcode (parse-input (slurp "resources/day9.input"))
        machine (init-machine intcode [1])]
    (println "Is the intcode runner correct:" (first ((run-machine machine) :output)))))
