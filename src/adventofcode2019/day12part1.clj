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

(def test-moons
  [{:x -1, :y 0, :z 2}
   {:x 2, :y -10, :z -7}
   {:x 4, :y -8, :z 8}
   {:x 3, :y 5, :z -1}])

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

(defn update-gravity [moon-state moon-states]
  (reduce (fn [result next-moon]
            (update-velocity (assoc moon-state :velocity (result :velocity)) next-moon))
          (cons moon-state moon-states)))

(defn update-moon-state [moon-state moon-states]
  (update-coordinates (update-gravity moon-state moon-states)))

(defn all-besides [l i]
  (into (vec (drop (+ i 1) l)) (vec (reverse (vec (take i l))))))

(defn update-all-moons-once [moon-states]
  (let [four-times (take 4 (iterate inc 0))]
    (vec (map (fn [i]
                (update-moon-state
                  (nth moon-states i)
                  (all-besides (vec moon-states) i)))
              four-times))))

(defn update-all-moons [moon-states n]
  (nth (iterate (partial update-all-moons-once) moon-states) n))

(defn calculate-energy [{:keys [coordinates velocity]}]
  (letfn [(energy [c] (reduce + (map (fn [x] (Math/abs ^Integer (val x))) c)))]
    (* (energy coordinates) (energy velocity))))

(defn calculate-total-energy [moon-states]
  (reduce + (map calculate-energy moon-states)))


; from here below: part 2

(defn contains-val-for-key-rec [[li & ls] x n]
  (cond
    (nil? li) nil
    (= li x) n
    :else (recur ls x (+ n 1))))

(defn nth-if-contained [l x]
  (contains-val-for-key-rec l x 0))

(defn does-history-repeat-itself? [[moon-states & _ :as all-moon-states] n]
  (let [next-moon-states (update-all-moons-once moon-states)]
    (if-let [ni (nth-if-contained all-moon-states next-moon-states)]
      (do
        (println n ni)
        (+ 1 ni))
      (recur (cons next-moon-states all-moon-states) (+ n 1)))))

; Es muessen sowohl die Koordinaten als auch die Geschwindigkeiten gleich sein.
; D.h. also man muss einen Kreis finden im System.
; Dann bilden aber auch die jeweilige Koordinaten mit der jeweiligen Geschwindigkeit einen Kreis,
; weil ja nur die voneinander abhängen. D.h. ich kann jetzt die Schritte n_x finden, wann sich
; also (x, v_x) wiederholt und genauso n_y und n_z. Die Gesamtschritte bis sich das gesamte
; System wiederholt ist dann kgV(n_x, n_y, n_z).

(defn -main []
  (let [moons moons
        ;moons test-moons

        moon-states (init-moon-states moons)
        ;later-moon-states (update-all-moons moon-states 2772)
        ]
    (do

      ;(println later-moon-states)
      ;(println later-moon-states)
      (println (does-history-repeat-itself? [moon-states] 0))
      )))
