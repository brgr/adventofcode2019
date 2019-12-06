(ns adventofcode2019.core
  (:require [clojure.string :as str]))

(defn read_csv_as_string_single_line [csv_text]
  (str/split csv_text #"\,"))

(defn read_csv_as_string_multiple_lines [csv_text]
  (mapv read_csv_as_string_single_line (str/split csv_text #"\n")))

