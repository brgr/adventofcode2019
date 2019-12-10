(ns adventofcode2019.day6part2
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(defn from [[from_node to_node]] from_node)
(defn to [[from_node to_node]] to_node)

(defn edges_starting [orbit_map from_node]
  (filter #(= from_node (from %)) orbit_map))

(defn successors [orbit_map from_node]
  (map #(to %) (edges_starting orbit_map from_node)))

(defn reaches? [orbit_map goal_node start_node]
  (let [successors (successors orbit_map start_node)]
    (if (nil? successors)
      false
      (do
        (if (some #(= goal_node %) successors)
          true
          (some true? (map (partial reaches? orbit_map goal_node) successors)))))))

(defn latest_connection_rec [orbit_map node1 node2 current_node]
  (if (and (reaches? orbit_map node1 current_node) (reaches? orbit_map node2 current_node))
    (let [successors (successors orbit_map current_node)
          latest_connections (map (partial latest_connection_rec orbit_map node1 node2) successors)
          filtered (filter #(not= nil %) latest_connections)]
      (if (empty? filtered)
        current_node
        (first filtered)))
    nil))

(defn latest_connection [orbit_map node1 node2]
  (latest_connection_rec orbit_map node1 node2 "COM"))

(defn steps [orbit_map goal_node start_node]
  (if (= start_node goal_node)
    0
    (let [successors (successors orbit_map start_node)
          filtered_successors (filter (partial reaches? orbit_map goal_node) successors)
          successor (first filtered_successors)]
      (do
        (if (some #(= goal_node %) successors)
          1
          (+ 1 (steps orbit_map goal_node successor)))))))

(defn orbital_transfers_between [orbit_map node1 node2]
  (let [latest_connection (latest_connection orbit_map node1 node2)
        steps_to_node1 (steps orbit_map node1 latest_connection)
        steps_to_node2 (steps orbit_map node2 latest_connection)]
    (- (+ steps_to_node1 steps_to_node2) 2)))

(defn -main []
  (let [input (slurp "resources/day6.input")
        ;input "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"
        orbit_map (map (fn [x] (str/split x #"\)")) (str/split input #"\n"))]
    (do
      (println orbit_map)
      (println (orbital_transfers_between orbit_map "YOU" "SAN")))))
