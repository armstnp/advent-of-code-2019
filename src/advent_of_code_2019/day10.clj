(ns advent-of-code-2019.day10
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (->> "day10.txt" core/read-input str/split-lines (mapv vec)))

(defn width
  [input]
  (count (first input)))
(defn height
  [input]
  (count input))

(defn map->asteroids
  [input]
  (for [x (range 0 (width input))
        y (range 0 (height input))
        :when (= \# ((input y) x))]
    [x y]))

(defn best-candidate
  [input]
  (let [asteroids (map->asteroids input)
        asteroid-lines (for [station asteroids
                             target asteroids
                             :when (not= station target)]
                         [station (:theta (polar station target))])]
    (->> asteroid-lines
         distinct
         (map first)
         frequencies
         (apply max-key second))))

(second (best-candidate input))

(def station (first (best-candidate input)))

(defn polar
  [[sx sy :as source] [tx ty :as target]]
  {:r (apply + (map (comp #(Math/abs %) -) source target)) ;; Close enough for our purposes
   :theta (- Math/PI (apply #(Math/atan2 %1 %2) ^:double (map (comp double -) target source)))
   :asteroid target})

(defn polar-lines
  [input station]
  (let [asteroids (map->asteroids input)
        polars (for [target asteroids
                     :when (not= station target)]
                 (polar station target))]
    (->> polars
         (group-by :theta)
         (sort-by first)
         (map (comp (partial map :asteroid) (partial sort-by :r) second)))))

(defn laser-until
  [[[next-asteroid & rest-asteroids] & rest-lines] n]
  (if (= n 1)
    next-asteroid
    (let [line' (if (empty? rest-asteroids) [] [rest-asteroids])
          lines' (concat rest-lines line')]
      (recur lines' (dec n)))))


(let [input input]
  (-> input
      (polar-lines station)
      (laser-until 200)))
