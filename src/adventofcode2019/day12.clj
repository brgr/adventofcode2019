(ns adventofcode2019.day12
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
    (= li x) (do (println "found!") (+ 1 n))
    :else (recur ls x (+ n 1))))

(defn nth-if-contained [l x]
  (contains-val-for-key-rec l x 0))

(defn get-coordinates-and-velocity-for-key [moon-states k]
  (map (fn [ms] [((ms :coordinates) k) ((ms :velocity) k)]) moon-states))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn least-common-multiple [x y z]
  (let [first (/ (* x y) (gcd x y))]
    (/ (* first z) (gcd first z))))

(defn does-history-repeat-itself?
  [[moon-states & _ :as all-moon-states] [first-x-states first-y-states first-z-states] [x-result y-result z-result] n]
  (if (not (or (nil? x-result) (nil? y-result) (nil? z-result)))
    (least-common-multiple x-result y-result z-result)
    (let [next-moon-states (update-all-moons-once moon-states)
          next-x-states (get-coordinates-and-velocity-for-key next-moon-states :x)
          next-y-states (get-coordinates-and-velocity-for-key next-moon-states :y)
          next-z-states (get-coordinates-and-velocity-for-key next-moon-states :z)]
      (let [xi (or x-result (if (= next-x-states first-x-states) (+ 1 n) nil))
            yi (or y-result (if (= next-y-states first-y-states) (+ 1 n) nil))
            zi (or z-result (if (= next-z-states first-z-states) (+ 1 n) nil))]
        (recur (cons next-moon-states all-moon-states)
               [first-x-states first-y-states first-z-states]
               [xi yi zi]
               (+ n 1))))))


(defn -main []
  (let [moons moons
        ;moons test-moons

        moon-states (init-moon-states moons)
        ;later-moon-states (update-all-moons moon-states 2772)
        ]
    (do

      ;(println later-moon-states)
      (println (does-history-repeat-itself?
                 [moon-states]
                 [(get-coordinates-and-velocity-for-key moon-states :x)
                  (get-coordinates-and-velocity-for-key moon-states :y)
                  (get-coordinates-and-velocity-for-key moon-states :z)]
                 [nil nil nil] 0))
      )))

; Explanation (in german)
; Ok, also dadurch dass ja Koordinaten+Velocities gleich sein müssen sind diese beiden States in einem Kreis zueinander. D.h. alle states die dazwischen sind auch davor und danach. Das muss deshalb so sein, weil wir ja sowohl f (also die Funktion, die den nächsten state definiert) als auch f^(-1) (also die Umkehrfunktion, d.h. die Funktion die den vorherigen state definiert) beide wohldefiniert haben.
;
;Daraus folgt, dass der erste State der wieder gleich ist ganz sicher dein allererster state war, da die ja, wie gesagt, in einem Kreis sind. Somit musst du immer nur mehr mit dem ersten State vergleichen und nicht mit allen allen States dazwischen (womit du dir ja schonmal mega viel Rechenleistung sparst!).
;
;Dazu kommt auch noch, dass du das kgV nutzen kannst, u…
;Wenn sich zB. (x,vx) nach 3 Schritten wiederholt und (y,vy) nach 5, dann wiederholen sich ((x,vx), (y,vy)), d.h. die beiden zusammen, beim kgV dieser Schritte, also nach 15 Schritten. Das leuchtet denk ich ein: (x,vx) wiederholt bei: 3, 6, 9, 12, 15, 18, .... und (y, vy) bei: 5, 10, 15, 20, ...
;Also sind sie alle beide das erste mal bei Schritt 15 (= kgv(3,5)) gleich. Wenn jetzt noch (z,vz) dazu kommt dann halt das kgv von allen drei.
;D.h. mit diesen zwei Tricks (also dass es allgemein komplett zyklisch ist und man nur mehr den ersten State checken muss, und dass man die unabhängig checken und dann das kgv bilden kann) schafft man das jetzt dann mit ner sehr guten Laufzeit zu berechnen :)
