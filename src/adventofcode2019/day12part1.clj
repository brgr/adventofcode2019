(ns adventofcode2019.day12part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(def moons
  [{:x 14, :y 9, :z 14}
   {:x 9, :y 11, :z 6}
   {:x -6, :y 14, :z -4}
   {:x 4, :y -4, :z -3}])

(defn init-moon-states [moons]
  (map (fn [m] {:coordinates m, :velocity {:x 0, :y 0, :z 0}}) moons))

(defn update-velocity
  [{coordinates1 :coordinates, velocity1 :velocity} {coordinates2 :coordinates}]
  (letfn [(update-value [k]
            (let [c1 (coordinates1 k) c2 (coordinates2 k)
                  v1 (velocity1 k) v2 (velocity1 k)]
              (cond
                (> c1 c2) (- v1 1)
                (< c1 c2) (+ v1 1)
                :else v1)))]
    {:coordinates coordinates1,
     :velocity    {:x (update-value :x), :y (update-value :y), :z (update-value :z)}}))

(defn update-coordinates [{:keys [coordinates velocity]}]
  {:coordinates (into {} (map (fn [[k1 v1] [_ v2]] [k1 (+ v1 v2)]) coordinates velocity)),
   :velocity    velocity})

(defn update-gravity [moon moons]
  (reduce (fn [result next-moon]
            (update-velocity (assoc moon :velocity (result :velocity)) next-moon))
          (conj moons moon)))

; TODO:
;  1. one round only: update-gravity for each moon with, respectively, each other moon, then update coordinates of
;     each moon
;  2. n rounds: do (1) for n rounds
;  3. calculate the total energy in the system


(defn -main []
  (let [input (slurp "resources/day12.input")]
    (do
      (println "TODO"))))
