(ns advent-of-code-2019.day1
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input
  (->> "day1.txt"
       core/read-input
       str/split-lines
       (map core/parse-int)))

;; Part 1 Solution

(def fuel-for-mass #(- (quot % 3) 2))

(->> input
     (map fuel-for-mass)
     (reduce +))

;; Part 2 Solution

(defn compound-fuel-for-mass
  [n]
  (->> n
       (iterate fuel-for-mass)
       (drop 1)
       (take-while #(> % 0))
       (reduce +)))

(->> input
     (map compound-fuel-for-mass)
     (reduce +))
