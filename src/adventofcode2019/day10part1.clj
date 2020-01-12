(ns adventofcode2019.day10part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(defn asteroid? [universe-map x y]
  (= \# (nth (nth universe-map y) x)))

(defn asteroids [universe-map]
  (for [y (range (count universe-map))
        x (range (count (first universe-map)))
        :when (asteroid? universe-map x y)]
    [x y]))

(defn unit-vector [[x y]]
  (let [divisor (math/sqrt (+ (* x x) (* y y)))]
    [(/ x divisor) (/ y divisor)]))

(defn unit-vector-from [[x-from y-from] [x-to y-to]]
  (let [x (- x-to x-from)
        y (- y-to y-from)]
    (unit-vector [x y])))

(defn =vec [[x1 y1] [x2 y2]]
  (let [epsilon 1e-10]
    (and
      (< x1 (+ x2 epsilon))
      (> x1 (- x2 epsilon))
      (< y1 (+ y2 epsilon))
      (> y1 (- y2 epsilon)))))

(defn distance [[x1 y1] [x2 y2]]
  (let [x-distance (math/abs (- x1 x2))
        y-distance (math/abs (- y1 y2))]
    (math/sqrt (+ (* x-distance x-distance) (* y-distance y-distance)))))

(defn blocking-sight?
  "Return whether the 3rd point is blocked in sight by the 2nd point"
  [p1 p2 p3]
  (and (=vec (unit-vector-from p1 p2) (unit-vector-from p1 p3))
       (> (distance p1 p3) (distance p1 p2))))

(defn remove-asteroids-out-of-sight [from to asteroids-in-sight]
  (remove (partial blocking-sight? from to) asteroids-in-sight))

(defn find-asteroids-from-rec [asteroid [other & others] results]
  (if (nil? other)
    results
    (recur asteroid others (remove-asteroids-out-of-sight asteroid other results))))

(defn find-asteroids-from [asteroids asteroid]
  (let [asteroids (remove #{asteroid} asteroids)]
    (find-asteroids-from-rec asteroid asteroids asteroids)))

(defn find-asteroid-which-sees-most-other-asteroids [asteroids]
  (map (comp count (partial find-asteroids-from asteroids)) asteroids))

(defn transform_input [input]
  (str/split input #"\n"))

(defn -main []
  (let [input (slurp "resources/day10.input")

        ;; Test inputs
        ; Best is [3 4] with 8 asteroids found
        ;input ".#..#\n.....\n#####\n....#\n...##\n"

        ; Best is [5 8] with 33 asteroids found
        ;input "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"

        ; Best is [1 2] with 45 asteroids found
        ;input "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.\n"

        ; Best is [6 3] with 41 asteroids found
        ;input ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..\n"

        ; Best is [11 13] with 210 asteroids found
        ;input ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##\n"

        universe-map (transform_input input)
        asteroids (asteroids universe-map)
        ;asteroid (nth asteroids 9)
        ]
    (do
      (println universe-map)
      (println (asteroid? universe-map 1 0))
      (println "asteroids:" asteroids)
      ;(println "asteroid:" asteroid)
      ;(println (count (find-asteroids-from asteroids asteroid)))
      (println (apply max (find-asteroid-which-sees-most-other-asteroids asteroids)))
      )))
