(ns adventofcode2019.day13part1
  (:require [intcode])
  (:use [adventofcode2019.core]))

(defn parse-statements [[[x y tile-id] & screen-statements] block-positions]
  (do
    (println tile-id)
    (let [block-positions (if (= tile-id 2)
                            (conj block-positions [x y])
                            (disj block-positions [x y]))]
      (if (nil? screen-statements)
        block-positions
        (recur screen-statements block-positions)))))

(defn parse-output [arcade-output]
  (let [screen-statements (partition 3 arcade-output)]
    (parse-statements screen-statements #{})))

(defn -main []
  (let [input (slurp "resources/day13.input")
        intcode (intcode/parse-input input)
        arcade-cabinet (intcode/init-machine intcode)

        screen (parse-output ((intcode/run-machine arcade-cabinet) :output))]
    (do
      (println "screen:" screen)
      (println "count:" (count screen)))))