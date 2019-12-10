(ns adventofcode2019.day6part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(defn from [[from_node _]] from_node)
(defn to   [[_   to_node]] to_node)

(defn edges_starting [orbit_map from_node]
  (filter #(= from_node (from %)) orbit_map))

(defn orbit_count_checksum [orbit_map count_predecessors from_node]
  (let [edges (edges_starting orbit_map from_node)
        successors (map #(to %) edges)
        checksum (count edges)]
    (if (= 0 checksum)
      count_predecessors
      (+ count_predecessors
         (reduce + (mapv (partial orbit_count_checksum orbit_map (+ count_predecessors 1)) successors))))))

(defn -main []
  (let [input (slurp "resources/day6.input")
        ;input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
        orbit_map (map (fn [x] (str/split x #"\)")) (str/split input #"\n"))]
    (do
      (println orbit_map)
      (println (orbit_count_checksum orbit_map 0 "COM")))))
