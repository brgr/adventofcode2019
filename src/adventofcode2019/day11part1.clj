(ns adventofcode2019.day11part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [intcode])
  (:use [adventofcode2019.core]))

; In the hash array, the colors of the emergency hull are indexed via their coordinates. At the beginning, all fields
; are black.
(def emergency-hull {[0 0] :black})


(defn get-color [emergency-hull coordinates]
  (let [result (get emergency-hull coordinates)]
    (if (nil? result) :black result)))

(defn color-code [color]
  (case color
    :black 0
    :white 1))


(defn run-painting-robot [intcode]
  ; at the start, the painting robot is on a black cell,
  (let [painting-robot (intcode/init-machine intcode [(color-code :black)])]
    ; TODO: given the output, do this recursively now according to the task
    ((intcode/run-machine painting-robot) :output)))

(defn -main []
  (let [input (slurp "resources/day11.input")
        intcode (intcode/parse-input input)]
    (do
      (println "Input:" input)
      (println "Intcode:" intcode)
      ; The following should return two outputs (see description of day 11)
      (println "Running painting robot:" (run-painting-robot intcode)))))
