(ns adventofcode2019.day19
  (:require [intcode])
  (:use [adventofcode2019.core]))

(def input (slurp "resources/day19.input"))
(def intcode (intcode/parse-input input))
(def machine (intcode/init-machine intcode))

(->> (intcode/run-machine machine)
     :input)

(for [y (range 50)]
  (apply str (for [x (range 50)
         :let [output (->> (intcode/set-input machine [x y])
                           (intcode/run-machine)
                           :output
                           (first))]]
     (if (zero? output)
       \.
       \#))))

(defn is-visible-at? [x y]
  (-> (intcode/set-input machine [x y])
      (intcode/run-machine)
      :output
      (first)))

(defn find-start-of-visible-space
  ([y] (find-start-of-visible-space y 0))
  ([y start-x] (loop [x start-x]
                 (let [at-x (is-visible-at? x y)]
                   (if (= 1 at-x)
                     x
                     (recur (+ x 1)))))))

(defn find-end-of-visible-space
  ([y] (find-end-of-visible-space y (find-start-of-visible-space y)))
  ([y start-x] (loop [x start-x]
                 (let [at-x (is-visible-at? x y)]
                   (if (= 0 at-x)
                     (- x 1)
                     (recur (+ x 1)))))))

(find-end-of-visible-space 1100)
(find-start-of-visible-space 1200)

(let [y 554]
  [(- (find-end-of-visible-space y) 1)
   (find-start-of-visible-space (+ y 99))])

; Notes:
; last-end for this is 435
;
; currently not found up until 1100

; e.g. the first 3x3 is at: y=12, x=21, starting count at 0
; first 4x4 is at: x=30, y=17
; first 5x5 is at: x=41, y=23

(find-start-of-visible-space 7)
(find-end-of-visible-space 7)

(def wanted-size 100)
(def start-y 500)

(loop [y start-y
       last-end (+ 1 (find-start-of-visible-space (- start-y 1)))]
  (let [x-end (find-end-of-visible-space y last-end)
        y-lower (+ y (- wanted-size 1))
        x-start (- x-end (- wanted-size 1))
        lower-x-start (find-start-of-visible-space y-lower (- x-start 1))]
    (if (= (+ 1 lower-x-start) x-start)
      [lower-x-start y]
      (recur (+ y 1) x-end))))

(def result-point *1)
(-> result-point)
(-> (+ (second result-point) (* 10000 (first result-point))))
