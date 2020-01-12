(ns adventofcode2019.day10part2
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

(defn =epsilon [a b]
  (let [epsilon 1e-10]
    (and (< a (+ b epsilon))
         (> a (- b epsilon)))))

(defn =vec [[x1 y1] [x2 y2]]
  (and (=epsilon x1 x2)
       (=epsilon y1 y2)))

(defn distance [[x1 y1] [x2 y2]]
  (let [x-distance (math/abs (- x1 x2))
        y-distance (math/abs (- y1 y2))]
    (math/sqrt (+ (* x-distance x-distance) (* y-distance y-distance)))))

(defn angle
  "The angle for the given vector, starting upwards, turning clockwise until 2*PI"
  [x y]
  (cond
    (>= x 0) (Math/atan2 x y)
    :else (- (* 2 (Math/PI)) (- (Math/atan2 x y)))))

(defn angle-from-to [[x-from y-from] [x-to y-to]]
  ; I thought the y-axis in reverse, so here I fix this sign error
  (let [x (- x-to x-from)
        y (- y-from y-to)]
    (angle x y)))

(defn hash-map-with-all-rec [[v & vs] m]
  (if (nil? v)
    m
    (let [v [(first v) [(second v)]]]
      (recur vs (merge-with into m (apply assoc {} v))))))

(defn hash-map-with-all
  "Transforms a vector into a hashmap, but summarizes values of the same key instead of dismissing
  all but one of them"
  [v]
  (into (sorted-map) (hash-map-with-all-rec v {})))

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
  (map
    (comp (fn [[asteroid found-asteroids]]
            [asteroid (count found-asteroids)])
          (fn [asteroid]
            [asteroid (find-asteroids-from asteroids asteroid)]))
    asteroids))

(defn all-angles-from [monitoring-station asteroids]
  (sort (map (fn [asteroid]
               [(angle-from-to monitoring-station asteroid) asteroid])
             asteroids)))

(defn next-asteroids [asteroid-angles last-angle]
  (let [next-asteroids (first (filter (fn [x] (> (key x) last-angle)) asteroid-angles))]
    (if (nil? next-asteroids)
      [-0.1 nil]
      next-asteroids)))

(defn nearest-asteroid [monitoring-station asteroids]
  (if (empty? asteroids)
    nil
    (apply min-key #(distance monitoring-station %) asteroids)))

(defn destroy-n-asteroids [monitoring-station asteroid-angles last-angle n]
  (let [[angle asteroids] (next-asteroids asteroid-angles last-angle)
        nearest-asteroid (nearest-asteroid monitoring-station asteroids)]
    (cond
      (and (empty? asteroids) (zero? n)) (recur monitoring-station (dissoc asteroid-angles angle) angle n)
      (zero? n) (do (println asteroids) nearest-asteroid)
      (empty? asteroids) (recur monitoring-station (dissoc asteroid-angles angle) angle n)
      :else (let [asteroid-angles (update asteroid-angles angle (partial remove #(= nearest-asteroid %)))]
              (recur monitoring-station asteroid-angles angle (- n 1))))))

(defn find-nth-destroyed-asteroid
  "Finds the nth asteroid that is destroyed. Note: n starts counting at 0, so n=199, e.g., finds the
  200th asteroid."
  [monitoring-station asteroids n]
  (let [asteroid-angles (hash-map-with-all (all-angles-from monitoring-station asteroids))]
    (destroy-n-asteroids monitoring-station asteroid-angles -0.1 n)))

(defn transform_input [input]
  (str/split input #"\n"))

(defn -main []
  (let [input (slurp "resources/day10.input")

        ;; Test inputs
        ; this should give the following asteroids:
        ;The 1st asteroid to be vaporized is at 11,12.
        ;The 2nd asteroid to be vaporized is at 12,1.
        ;The 3rd asteroid to be vaporized is at 12,2.
        ;The 10th asteroid to be vaporized is at 12,8.
        ;The 20th asteroid to be vaporized is at 16,0.
        ;The 50th asteroid to be vaporized is at 16,9.
        ;The 100th asteroid to be vaporized is at 10,16.
        ;The 199th asteroid to be vaporized is at 9,6.
        ;The 200th asteroid to be vaporized is at 8,2.
        ;The 201st asteroid to be vaporized is at 10,9.
        ;The 299th and final asteroid to be vaporized is at 11,1.
        ;input ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"


        universe-map (transform_input input)
        ; this is the asteroid for the monitoring station, as found in part 1
        monitoring-station [26 28]

        ; for test input:
        ;monitoring-station [11 13]

        asteroids (remove #(= monitoring-station %) (asteroids universe-map))
        ]
    (do
      (println universe-map)
      (println (asteroid? universe-map 1 0))
      (println "asteroids:" asteroids)
      ;(println (find-asteroid-which-sees-most-other-asteroids asteroids))
      ;(println (all-angles-from monitoring-station asteroids))
      (println (find-nth-destroyed-asteroid monitoring-station asteroids 199))
      )))

; Results:

; 1614 ([16 14]) is TOO HIGH
; 1416 ([14 16]) is TOO HIGH
; 1005 ([10 05]) is TOO LOW (received with n=199)
; 1006 ([10 06]) is TOO LOW (n=201)