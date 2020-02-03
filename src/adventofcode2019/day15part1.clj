(ns adventofcode2019.day15part1
  (:require [intcode])
  (:use [adventofcode2019.core]))


(def moves {:up 1, :right 4, :down 2, :left 3})

(defn move-repair-droid [repair-droid direction]
  (let [repair-droid (intcode/conj-input repair-droid direction)
        repair-droid (intcode/run-machine repair-droid)]
    (intcode/remove-last-output repair-droid)))

(defn move-fully-to-one-side [repair-droid direction]
  (let [[repair-droid last-output] (move-repair-droid repair-droid direction)]
    (if (zero? last-output)
      repair-droid
      (recur repair-droid direction))))

(defn go-to-top-left-point-of-field [repair-droid]
  (let [[repair-droid last-output] (move-repair-droid repair-droid (moves :up))]
    (if (zero? last-output)
      (let [[repair-droid last-output] (move-repair-droid repair-droid (moves :left))]
        (if (zero? last-output)
          repair-droid
          (recur repair-droid)))
      (recur repair-droid))))

(defn str-area [hull]
  (let [lowest-x (apply min (map (fn [c] (first (key c))) hull))
        lowest-y (apply min (map (fn [c] (second (key c))) hull))
        hull (into {} (map (fn [c] (let [[x y] (key c)] [[(+ x (Math/abs ^Integer lowest-x)) (+ y (Math/abs ^Integer lowest-y))] (val c)])) hull))
        max-x (+ 1 (apply max (map (fn [c] (first (key c))) hull)))
        max-y (+ 1 (apply max (map (fn [c] (second (key c))) hull)))]
    (apply str (for [y (range max-y)]
                 (str (apply str (for [x (range max-x)]
                                   (let [v (get hull [x (- max-y y 1)])]
                                     (case v
                                       0 "#"
                                       1 " "
                                       2 "o"
                                       "x" "x"
                                       ".")))) "\n")))))

(defn position-after-move [[x y] move]
  (case move
    :up [x (+ y 1)]
    :down [x (- y 1)]
    :right [(+ x 1) y]
    :left [(- x 1) y]))

(defn get-droid-on-oxygen-station [droid area position]
  (reduce (fn [[droid area] move]
            (let [next-position (position-after-move position (key move))]
              (if (or (contains? area next-position) (some (fn [e] (= 2 (val e))) area))
                [droid area]
                (let [[new-droid output] (move-repair-droid droid (val move))
                      area (assoc area next-position output)]
                  (if (zero? output)
                    [droid area]
                    (let [[new-droid area] (get-droid-on-oxygen-station new-droid area next-position)]
                      (if (some (fn [e] (= 2 (val e))) area)
                        [new-droid area]
                        [droid area])))))))
          [droid area] moves))


(defn map-whole-field [droid area position distance]
  (do
    (println position)
    (if (not (empty? area)) (println (str-area (assoc area position "x"))))
    ;(read-line)

    (reduce (fn [[droid area highest-distance-so-far] move]
              (let [next-position (position-after-move position (key move))]
                (if (contains? area next-position)
                  [droid area highest-distance-so-far]
                  (let [[new-droid output] (move-repair-droid droid (val move))
                        area (assoc area next-position output)]
                    (if (zero? output)
                      [droid area highest-distance-so-far]
                      (let [[_ area new-highest-distance] (map-whole-field new-droid area next-position (+ distance 1))]
                        (if (> new-highest-distance highest-distance-so-far)
                          [droid area new-highest-distance]
                          [droid area highest-distance-so-far])))))))
            [droid area distance] moves)))

(defn read-move []
  (let [input (read-line)]
    (case input
      "w" :up
      "a" :left
      "s" :down
      "d" :right
      (recur))))

(defn map-interactive [droid area position]
  (do
    (if (not (empty? area)) (println (str-area (assoc area position "x"))))

    (let [next-move (read-move)
          next-position (position-after-move position next-move)
          [droid output] (move-repair-droid droid (moves next-move))
          area (assoc area next-position output)]

      (if (zero? output)
        (recur droid area position)
        (recur droid area next-position))
      )))

(defn run-around-whole-field [intcode]
  (let [repair-droid (intcode/init-machine intcode)
        ;repair-droid (go-to-top-left-point-of-field repair-droid)
        ;[repair-droid _] (move-repair-droid repair-droid (move :right))
        ;[repair-droid first-field] (move-repair-droid repair-droid (move :left))
        ]
    (do
      ;(println first-field)
      (map-whole-field repair-droid {} [0 0] 0))))


(def input (slurp "resources/day15.input"))
(def test-input "3,70,9,70,2001,71,74,73,2001,72,78,74,1002,70,-1,70,9,70,2,73,100,83,1,83,74,83,9,83,1201,116,0,84,1002,83,-1,83,9,83,1006,84,49,1001,73,0,71,1001,74,0,72,4,84,1105,1,0,99,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-1,1,0,0,0,0,-1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6,-1,0,0,-1,-1,-1,0,1,1,0,0,-1,0,1,0,1,1,0,0,1,2,1,0,-1,-1,0,0,0,-1,-1")

(defn -main []
  (let [intcode (intcode/parse-input input)
        ;[repair-droid area] (run-around-whole-field intcode)
        [repair-droid area] (get-droid-on-oxygen-station (intcode/init-machine intcode) {} [0 0])
        [repair-droid area max-distance] (map-whole-field repair-droid {} [0 0] 0)
        ]
    (do
      (println (str-area area))
      ;(println (second (move-repair-droid (first (move-repair-droid repair-droid (moves :right))) (moves :left))))
      ;(println repair-droid)
      (println "max distance found:" max-distance)

      )))

; Incorrect answers for part 2:
; 799 too high
; 520 (also 519) too high