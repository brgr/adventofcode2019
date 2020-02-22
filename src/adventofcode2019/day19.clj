(ns adventofcode2019.day19
  (:require [intcode])
  (:use [adventofcode2019.core]))

(def input (slurp "resources/day19.input"))
(def intcode (intcode/parse-input input))
(def machine (intcode/init-machine intcode))

(->> (intcode/run-machine machine)
     :input)

(count (for [x (range 50) y (range 50)
             :let [output (->> (intcode/init-machine intcode [x y])
                               (intcode/run-machine)
                               :output)]
             :when (= (first output) 1)]
         1))