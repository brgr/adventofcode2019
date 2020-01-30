(ns adventofcode2019.day13part2
  (:require [intcode])
  (:use [adventofcode2019.core]))

(def width 43)
(def height 22)

(def symbols [\  \# \~ \= \o])

(def init-display {:screen         (vec (take height (repeat (vec (take width (repeat 0))))))
                   :score          nil
                   :ball-position  nil
                   :board-position nil})

(defn get-screen-size-rec [[[x y] & screen-statements] [width height]]
  (let [width (if (> x width) x width)
        height (if (> y height) y height)]
    (if (nil? screen-statements) [width height] (recur screen-statements [width height]))))

(defn get-screen-size [screen-statements]
  (get-screen-size-rec screen-statements [0 0]))

(defn parse-game-display-rec
  [[[x y v] & display-statements] {:keys [screen score ball-position board-position] :as display}]
  (if (= [x y] [-1 0])
    ; the score is always at the last position
    (let [display (assoc display :score v)]
      (if (nil? display-statements)
        display
        (recur display-statements display)))
    (let [screen-line (nth screen y)
          screen (assoc screen y (assoc screen-line x (nth symbols v)))
          display {:screen         screen
                   :score          score
                   :ball-position  (if (= v 4) [x y] ball-position)
                   :board-position (if (= v 3) [x y] board-position)}]
      (if (nil? display-statements)
        display
        (recur display-statements display)))))

(defn parse-game-display [display-statements display]
  (do
    (println display-statements)
    (parse-game-display-rec display-statements display)))

(defn str-screen [screen]
  (apply str (map (fn [s] (apply str (conj s "\n"))) screen)))

(defn count-obstacles [{:keys [screen]}]
  (reduce + (map (fn [l] (reduce (fn [acc i] (if (= i \~) (+ acc 1) acc)) 0 l)) screen)))

(defn str-display [display]
  (str "Score: " (display :score) "\n"
       "Ball position:  " (display :ball-position) "\n"
       "Board position: " (display :board-position) "\n"
       "Remaining obstacles: " (count-obstacles display) "\n"
       "\n" (str-screen (display :screen))))

(defn display-statements [arcade-output]
  (partition 3 arcade-output))

(defn run-game [[arcade-cabinet display]]
  (let [arcade-cabinet (intcode/run-machine arcade-cabinet)
        display (parse-game-display (display-statements (arcade-cabinet :output)) display)
        x-board (nth (display :board-position) 0)
        x-ball (nth (display :ball-position) 0)
        next-input (cond
                     (= x-board x-ball) 0
                     (> x-board x-ball) -1
                     :else 1)]

    (do
      (println (str-display display))
      [(intcode/conj-input (intcode/clear-output arcade-cabinet) next-input) display])))

(defn -main []
  (let [input (slurp "resources/day13.input")
        intcode (intcode/parse-input input)
        intcode (assoc intcode 0 2)
        arcade-cabinet (intcode/init-machine intcode)]
    (doall
      ; the number below was found out by bisecting with trial-and-error :-)
      ;(take 4082 (iterate run-game [arcade-cabinet init-display]))

      ; the nicer way:
      (take-while
        (fn [[_ display]] (or (nil? (display :score)) (> (count-obstacles display) 0)))
        (iterate run-game [arcade-cabinet init-display]))
      )))