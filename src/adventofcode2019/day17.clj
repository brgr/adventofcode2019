(ns adventofcode2019.day17
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set]
            [intcode])
  (:use [adventofcode2019.core]))

(defn at-position [area [x y]]
  (nth (nth area y) x))

(defn is-intersection? [area [x y]]
  (and (not (or (zero? x)
                (zero? y)
                (= (+ 1 y) (count area))
                (= (+ 1 x) (count (first area)))))
       (= \#
          (at-position area [x (- y 1)])
          (at-position area [x (+ y 1)])
          (at-position area [(+ x 1) y])
          (at-position area [(- x 1) y])))
  )

(defn intersections [area]
  (apply concat (map #(remove nil? %) (let [area (str/split area #"\n")]
                                        (for [y (range (count area))
                                              :let [line (nth area y)]]
                                          (for [x (range (count line))]
                                            (if (is-intersection? area [x y])
                                              ;"O"
                                              [x y]
                                              ;(get line x)
                                              )))))))

(defn get-lines-from-intersection [area [x y]]
  ; todo: area is currently one string; starting from [x y], find the horizontal and vertical line surrounding it
  ; this maybe trespasses another intersection; don't just use max x/y, as there could be another line later on!
  )

(defn get-lines-rec [area [[x y] & intersections] lines]
  (let [[line1 line2] (get-lines-from-intersection area [x y])
        lines (conj lines line1 line2)]
    (if (nil? intersections)
      lines
      (recur area intersections lines))))

(defn get-lines [area intersections]
  (get-lines-rec area intersections []))

(defn -main []
  (let [input (slurp "resources/day17.input")
        intcode (intcode/parse-input input)

        machine (intcode/init-machine intcode)
        output ((intcode/run-machine machine) :output)
        area (->> output
                  (map char)
                  (apply str))]
    (do
      (println area)
      (println (intersections area))

      )))
