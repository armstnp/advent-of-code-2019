(ns advent-of-code-2019.day8
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (-> "day8.txt" core/read-input str/trim))
(def width 25)
(def height 6)
(def layer-area (* width height))

(defn num-layers
  [input]
  (/ (count input) layer-area))

(defn count-char
  [c layer]
  (count (filter (partial = c) layer)))

(->> input
     (partition layer-area)
     (apply min-key (partial count-char \0))
     ((juxt (partial count-char \1) (partial count-char \2)))
     (apply *))

(defn mix
  [c1 c2]
  (if (= \2 c1)
    c2
    c1))

(->> input
     (partition layer-area)
     (reduce (partial map mix))
     (map #(if (= \0 %) \space %))
     (partition width)
     (map (comp (partial apply str) vec))
     (str/join "\n")
     println)
