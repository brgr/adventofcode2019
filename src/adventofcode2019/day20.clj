(ns adventofcode2019.day20
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def simple-maze (slurp "resources/day20.input"))
;(def simple-maze "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       ")
;(def simple-maze "                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               ")

(def maze-area (str/split simple-maze #"\n"))
(def width (->> (nth maze-area 3) (count) (+ 2)))
(def height (count maze-area))

(defn is-door? [tile]
  (and (some? tile) (>= (int tile) (int \A)) (<= (int tile) (int \Z))))

(defn area-at
  ([x y] (-> (get maze-area y) (get x)))
  ([[x y]] (area-at x y)))

(defn adjacent-tiles
  ([x y] (letfn [(tile-at [tile position]
                   (if (= tile \.)
                     [:floor position]
                     (if (is-door? tile)
                       [(keyword (str tile)) position]
                       nil)))]
           (->> (for [step (range -1 2 2)
                      :let [position1 [(+ x step) y]
                            position2 [x (+ y step)]]]
                  (vector (tile-at (area-at position1) position1)
                          (tile-at (area-at position2) position2)))
                (apply concat)
                (remove (fn [v] (or (nil? v) (= ":" (first v))))))))
  ([[x y]] (adjacent-tiles x y)))

(defn adjacent-doors
  ([x y] (->> (try (adjacent-tiles x y)
                   (catch Exception e ()))
              (filter (fn [v] (not= (first v) :floor)))))
  ([[x y]] (adjacent-doors x y)))

(defn is-next-to-maze? [x y]
  (as-> (adjacent-tiles x y) tiles
        (into (hash-map) tiles)
        (select-keys tiles [:floor])
        (> (count tiles) 0)))

(defn portal-keyword [door1 door2]
  "Returns the unique portal keyword for door1 and door2. door1 and door2 must each be a char."
  (if (< (int door1) (int door2))
    (keyword (str door1 door2))
    (keyword (str door2 door1))))

(defn portal-at
  "x, y must be a location of a door (thus also of a portal), then this function returns the
   unique portal keyword of the portal at this position."
  ([x y] (let [door1 (area-at x y)
               door2 (->> (adjacent-doors x y) (first) (first) (str) (second))]
           (portal-keyword door1 door2)))
  ([[x y]] (portal-at x y)))

(def all-portals
  (->>
    (for [x (range width) y (range height)
          :let [tile (area-at x y)
                [door _] (first (adjacent-doors x y))]
          :when (and (is-door? tile)
                     (do (println x y (portal-at x y) (is-next-to-maze? x y))
                         (is-next-to-maze? x y)))]
      (let [portal (portal-keyword tile (first (name door)))]
        [portal [x y]]))
    (map (fn [e] (into (hash-map) (vector (vector (first e) (set (vector (second e))))))))
    (apply merge-with set/union)))

(def start-door (first (all-portals :AA)))

(defn doors-reachable-from [from-door]
  "from-door is the [x y] coordinates of the door where we start searching for the other doors it
   may reach. The [x y] coordinates need to be the coordinates of the door that are right next to
   the maze (i.e., (adjacent-tiles from-door) needs to return exactly one '.', i.e. the start of
   the path)"
  (letfn [(search-other-doors [current already-moved goals steps]
            (let [already-moved (conj already-moved current)
                  next-positions (as-> (adjacent-tiles current) ti
                                       (map second ti)
                                       (set ti)
                                       (set/difference ti already-moved))]
              (if (empty? next-positions)
                [goals already-moved]
                (reduce (fn [[goals already-moved] next-position]
                          (let [tile (area-at next-position)]
                            (if (= \. tile)
                              (search-other-doors next-position already-moved goals (+ steps 1))
                              (if (not= \# tile)
                                [(conj goals [next-position steps]) already-moved]))))
                        [goals already-moved] next-positions))))]
    (let [current (->> (adjacent-tiles from-door)
                       (filter (fn [x] (= :floor (first x))))
                       (first)
                       (second))
          already-moved #{from-door}
          goals []
          steps 0]
      (->> (search-other-doors current already-moved goals steps)
           (first)
           (map (fn [x]
                  {:portal      (portal-at (first x))
                   :coordinates (first x)
                   :steps       (second x)}))))))

(defn get-other-door [all-portals portal-keyword entry-coordinates]
  (->> (all-portals portal-keyword)
       (filter (partial not= entry-coordinates))
       (first)))

(defn find-smallest-steps-to-ZZ [current-portal-position steps smallest-steps-so-far]
  "current-portal-position shall be the [x y] position of a portal"
  (if (> steps smallest-steps-so-far)
    smallest-steps-so-far
    (let [all-next-portals (doors-reachable-from current-portal-position)
          steps-to-ZZ (if-let [filtered-portals (seq (filter (fn [x] (= :ZZ (x :portal))) all-next-portals))]
                        (-> (first filtered-portals) :steps)
                        nil)
          smallest-steps-so-far (if (nil? steps-to-ZZ)
                                  smallest-steps-so-far
                                  (if (< (+ steps steps-to-ZZ) smallest-steps-so-far)
                                    (+ steps steps-to-ZZ)
                                    smallest-steps-so-far))
          next-portals (filter (fn [p] (and (not= :AA (p :portal)) (not= :ZZ (p :portal)))) all-next-portals)]
      (reduce (fn [smallest-steps-so-far p]
                (let [other-door (get-other-door all-portals (p :portal) (p :coordinates))
                      steps (+ steps (p :steps) 1)
                      new-smallest-steps (find-smallest-steps-to-ZZ other-door steps smallest-steps-so-far)]
                  (if (< new-smallest-steps)
                    new-smallest-steps
                    smallest-steps-so-far)))
              smallest-steps-so-far next-portals))))

(find-smallest-steps-to-ZZ start-door 0 100000)





