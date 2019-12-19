(ns adventofcode2019.day8part2
  (:require [clojure.string :as str]
            [clojure.core.match :refer [match]]
            [clojure.math.numeric-tower :as math]
            [clojure.set :as set])
  (:use [adventofcode2019.core]))

(def width 25)
(def height 6)
;(def width 2)
;(def height 2)

(defn get_layers [image_data]
  (let [layers_unsplit (re-seq (re-pattern (str ".{1," (* height width) "}")) image_data)
        layers_with_string (mapv (fn [lus] (vec (re-seq (re-pattern (str ".{1," width "}")) lus))) layers_unsplit)]
    (mapv (fn [l] (mapv (fn [s]  (mapv (fn [x] (Integer/parseInt x)) (str/split s #""))) l)) layers_with_string)))

(defn image_to_string [image]
  (apply str (mapv (fn [i] (str (apply str i) "\n")) image)))

(defn transform_pixel [ipixel, lpixel]
  (if (= ipixel 2)
    lpixel
    ipixel))

(defn decode_image_rec [[layer & layers] image]
  (if (nil? layer)
    image
    (let [new_image (mapv #(mapv transform_pixel %1 %2) image layer)]
      (decode_image_rec layers new_image))))

(defn decode_image [layers]
  (let [image (vec (repeat height (vec (repeat width 2))))]
    (decode_image_rec layers image)))


(defn -main []
  (let [input (slurp "resources/day8.input")
        ;input "0222112222120000"

        layers (get_layers input)
        image (decode_image layers)]
    (do
      ;(println (count (first layers)))
      ;(mapv println layers)
      (println "layers:" layers)
      (println image)
      (println (image_to_string image))
      ;(println (image_to_string (decode_image layers)))
      )))

;0000  00    00 0  0 0000 
;0    0  0    0 0  0    0 
;000  0       0 0  0   0  
;0    0 00    0 0  0  0   
;0    0  0 0  0 0  0 0    
;0     000  00   00  0000
; = FGJUZ

; incorrect results:
; 222222222202222222221212012222202202221022222222222122220222202121002222222220202222222222222222222212222222222222022222200222222222220222222222222112
; 1111001100001101001011110 1000010010000101001000010 1110010000000101001000100 1000010110000101001001000 1000010010100101001010000 1000001110011000110011110
; 111100110000110100101111010000100100001010010000101110010000000101001000100100001011000010100100100010000100101001010010100001000001110011000110011110
