(ns adventofcode2019.day17
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
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
          (at-position area [(- x 1) y]))))

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

(def area
  (let [input (slurp "resources/day17.input")
        intcode (intcode/parse-input input)

        machine (intcode/init-machine intcode)
        output ((intcode/run-machine machine) :output)
        area (->> output
                  (map char)
                  (apply str))]
    area))

(def area-array (str/split area #"\n"))

(def intersections (intersections area))

(def width 39)
(def height 51)

(defn area-at [area [x y]]
  "This does currently not check for newline, which it gives back just as well."
  (try
    (let [field (get area (+ (* y (+ 1 width)) x))]
      (if (= field \newline)
        nil
        field))
    (catch Exception _ nil)))


(def starting-point
  (let [index (str/index-of area "^")
        x (mod index (+ 1 width))
        y (quot index (+ 1 width))]
    [x y]))

(def directions {:up 0 :right 1 :down 2 :left 3})

(defn turn [from-direction to-direction]
  (let [f-index (directions from-direction)
        t-index (directions to-direction)]
    (cond
      (= (mod (+ f-index 1) 4) t-index) :R
      (= (mod (- f-index 1) 4) t-index) :L)))

(defn turn-left [direction]
  (case direction
    :up :left
    :left :down
    :down :right
    :right :up))

(defn turn-right [direction]
  (case direction
    :up :right
    :right :down
    :down :left
    :left :up))

(defn is-scaffold-or-bot? [area [x y]]
  (some (partial = (area-at area [x y])) [\# \o \^ \< \> \v]))

(defn move-forward-as-far-as-possible [[x y] direction]
  (letfn [(fy [op] (op y (last (take-while
                                 (fn [yi] (is-scaffold-or-bot? area [x (op y yi)])) (range)))))
          (fx [op] (op x (last (take-while
                                 (fn [xi] (is-scaffold-or-bot? area [(op x xi) y])) (range)))))]
    (let [[[x y] steps] (case direction
                          :up (let [min-y (fy -)]
                                [[x min-y] (- y min-y)])
                          :left (let [min-x (fx -)]
                                  [[min-x y] (- x min-x)])
                          :down (let [max-y (fy +)]
                                  [[x max-y] (- max-y y)])
                          :right (let [max-x (fx +)]
                                   [[max-x y] (- max-x x)]))]
      (if (zero? steps) nil [[x y] steps]))))

(defn left-from-position [position direction]
  (let [new-direction (turn-left direction)]
    (if-let [[new-position steps] (move-forward-as-far-as-possible position new-direction)]
      [new-position new-direction steps])))

(defn right-from-position [position direction]
  (let [new-direction (turn-right direction)]
    (if-let [[new-position steps] (move-forward-as-far-as-possible position new-direction)]
      [new-position new-direction steps])))

(defn next [position direction]
  (if-let [result (left-from-position position direction)]
    result
    (if-let [result (right-from-position position direction)]
      result)))

(defn get-turning-sequence [starting-point]
  (loop [current-position starting-point
         direction :up
         movements []]
    (let [[new-position new-direction steps-moved :as next] (next current-position direction)]
      (if (nil? next)
        movements
        (recur new-position
               new-direction
               (conj movements [(turn direction new-direction) steps-moved]))))))

(get-turning-sequence starting-point)

(-> area)
(def turning-sequence *1)

(replace {[[:L 4] [:L 6]] :A} turning-sequence)


; Trying out by hand I found out the following:
;C =
;[:R 12]
;[:L 6]
;[:L 6]
;[:L 8]
;
;B =
;[:L 4]
;[:L 6]
;[:L 8]
;[:L 12]
;
;A =
;[:L 8]
;[:R 12]
;[:L 12]
;
; and the sequence is
; B A A B A C B C A C

(def main-movement-routine "B,A,A,B,A,C,B,C,A,C\n")
(def movement-function-a "L,8,R,12,L,12\n")
(def movement-function-b "L,4,L,6,L,8,L,12\n")
(def movement-function-c "R,12,L,6,L,6,L,8\n")
(def no-continuous-video-feed "n\n")

(def input-as-str
  (str main-movement-routine
       movement-function-a
       movement-function-b
       movement-function-c
       no-continuous-video-feed))

(def input-for-machine
  (->> (map char input-as-str) (map int)))


(defn -main []
  (let [input (slurp "resources/day17.input")
        intcode (intcode/parse-input input)
        intcode (assoc intcode 0 2)

        machine (intcode/init-machine intcode input-for-machine)]
    (do
      (println (->> (intcode/run-machine machine) :output))

      )))
