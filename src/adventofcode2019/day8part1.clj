(ns adventofcode2019.day8part1
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))


(defn get_layers [image_data]
  (map #(re-seq #".{1,6}" %) (re-seq #".{1,150}" image_data)))

(defn count_chars [char pixels]
  (count (re-seq char pixels)))

(defn count_chars_in_layer [char layer]
  (reduce + (map (partial count_chars char) layer)))

(defn layer_with_minimal_zeros [layers]
  (apply min-key (partial count_chars_in_layer #"0") layers)
  ;(count_chars_in_layer #"0" (first layers))
  )


(defn -main []
  (let [input (slurp "resources/day8.input")
        layers (get_layers input)
        layer_with_minimal_zeros (layer_with_minimal_zeros layers)]
    (do
      (println layers)
      (println layer_with_minimal_zeros)
      (println (* (count_chars_in_layer #"2" layer_with_minimal_zeros)(count_chars_in_layer #"1" layer_with_minimal_zeros))))))
