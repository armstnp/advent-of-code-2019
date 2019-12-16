(ns advent-of-code-2019.day16
  (:require [advent-of-code-2019.core :as core]))

(def input (->> "day16.txt" core/read-input (map core/parse-int) drop-last))

(defn pattern
  [length width]
  (->> [0 1 0 -1]
       (mapcat (partial repeat width))
       cycle
       (drop 1)
       (take length)
       vec))

(defn pattern-block
  [input]
  (->> input
       count
       inc
       (range 1)
       (map (partial pattern (count input)))))

(defn abs
  [x]
  (if (< x 0) (- x) x))

(defn run-phase
  [signal]
  (->> signal
       pattern-block
       (map (fn [pattern]
              (->> pattern
                   (map * signal)
                   (reduce +)
                   (#(rem % 10))
                   abs)))))

(->> input (iterate run-phase) (drop 100) first (take 8))

(defn run-partial-sum-phase
  [rev-signal]
  (->> rev-signal
       (reductions +)
       (map (comp abs #(rem % 10)))))

(let [real-signal-size (* 10000 (count input))
      offset (->> input (take 7) (apply str) core/parse-int)
      remainder (- real-signal-size offset)
      mod-offset (mod offset (count input))]
  (->> input
       cycle
       (drop mod-offset)
       (take remainder)
       reverse
       (iterate run-partial-sum-phase)
       (drop 100)
       first
       (take-last 8)
       reverse))
