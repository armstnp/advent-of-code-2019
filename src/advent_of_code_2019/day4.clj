(ns advent-of-code-2019.day4
  (:require [advent-of-code-2019.core :as core]))

(def input-min 172930)
(def input-max 683082)

(defn two-adjacent?
  [n]
  (re-find #"(\d)(?:\1)" (str n)))

(defn not-decreasing?
  [n]
  (let [s (seq (str n))]
    (= s (sort s))))

(def password-p1? (every-pred two-adjacent? not-decreasing?))

(->> input-max
     inc
     (range input-min)
     (filter password-p1-re?)
     count)

(defn exactly-two-adjacent?
  [n]
  (re-find #"^(\d)(?:\1)(?!\1)|(\d)(?!\4)(\d)(?:\5)(?!\5)" (str n)))

(def password-p2? (every-pred exactly-two-adjacent? not-decreasing?))

(->> input-max
     inc
     (range input-min)
     (filter password-p2?)
     count)
