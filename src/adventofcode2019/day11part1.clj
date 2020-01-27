(ns adventofcode2019.day11part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [intcode])
  (:use [adventofcode2019.core]))

(defn get-color-code [emergency-hull coordinates]
  (let [result (get emergency-hull coordinates)]
    (if (nil? result) 0 result)))

(defn direction-code [direction]
  (case direction
    :up 0
    :right 1
    :down 2
    :left 3))

(defn turn-around [facing-direction turn-direction]
  "facing-direction: 0 is up, 1 is right, 2 is down, 3 is left"
  (case turn-direction
    ; turn left
    0 (mod (- facing-direction 1) 4)
    ; turn right
    1 (mod (+ facing-direction 1) 4)))

(defn move-forward [[x y] facing-direction]
  (case facing-direction
    ; up
    0 [x (+ y 1)]
    ; right
    1 [(+ x 1) y]
    ; down
    2 [x (- y 1)]
    ; left
    3 [(- x 1) y]))

(defn run-painting-robot-rec [painting-robot emergency-hull position facing-direction]
  (let [painting-robot (intcode/conj-input painting-robot (get-color-code emergency-hull position))
        painting-robot (intcode/run-machine painting-robot)]
    (if (= :stopped (painting-robot :state))
      emergency-hull
      (let [[painting-robot color-code] (intcode/remove-last-output painting-robot)
            [painting-robot turn-code] (intcode/remove-last-output painting-robot)
            ; above, is turn-code and color-code in the right order? (I think it is)
            emergency-hull (assoc emergency-hull position color-code)
            facing-direction (turn-around facing-direction turn-code)
            position (move-forward position facing-direction)]
        (recur painting-robot emergency-hull position facing-direction)))))

(defn run-painting-robot [intcode]
  ; at the start, the painting robot is on a black cell,
  (let [position [0 0]
        emergency-hull {position 1}
        painting-robot (intcode/init-machine intcode)]
    (run-painting-robot-rec painting-robot emergency-hull position (direction-code :up))
    ))


(defn draw-emergency-hull [hull]
  (let [lowest-x (apply min (map (fn [c] (first (key c))) hull))
        lowest-y (apply min (map (fn [c] (second (key c))) hull))
        hull (into {} (map (fn [c] (let [[x y] (key c)] [[(+ x (Math/abs ^Integer lowest-x)) (+ y (Math/abs ^Integer lowest-y))] (val c)])) hull))
        max-x (+ 1 (apply max (map (fn [c] (first (key c))) hull)))
        max-y (+ 1 (apply max (map (fn [c] (second (key c))) hull)))]
    (apply str (for [y (range max-y)]
                 (str (apply str (for [x (range max-x)]
                                   (let [v (get hull [x (- max-y y 1)])]
                                     (if (or (nil? v) (= v 0))
                                       " "
                                       "#")))) "\n")))))

(defn -main []
  (let [input (slurp "resources/day11.input")

        ; test input, this does as the example given in the description
        ;input "3,100,104,1,104,0,3,100,104,0,104,0,3,100,104,1,104,0,3,100,104,1,104,0,3,100,104,0,104,1,3,100,104,1,104,0,3,100,104,1,104,0,99"

        intcode (intcode/parse-input input)
        emergency-hull (run-painting-robot intcode)]
    (do
      (println "Input:" input)
      (println "Intcode:" intcode)
      (println "Running painting robot:" emergency-hull)
      (println "count emergency-hull:" (count emergency-hull))

      (println (draw-emergency-hull emergency-hull))
      )))

; Incorrect answers:
; 4
; 29
; 28
; 2136
; 2135
; 19
