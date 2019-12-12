(ns advent-of-code-2019.day12
  (:require [advent-of-code-2019.core :as core]
            [advent-of-code-2019.left-shark :as ls]
            [clojure.string :as str]))

(def input (->> "day12.txt" core/read-input str/split-lines))

(def parse-line
  (comp :components
     (ls/parse
       (ls/! "<x=")
       (ls/int! :x)
       (ls/! ", y=")
       (ls/int! :y)
       (ls/! ", z=")
       (ls/int! :z)
       (ls/! ">")
       ls/$)))

(defn lines->moons
  [input]
  (->> input
       (map parse-line)
       (map #(into {} [[:pos (vec ((juxt :x :y :z) %))] [:vel [0 0 0]]]))))

(defn apply-gravity
  [{[px1 py1 pz1] :pos :as primary-moon} {[px2 py2 pz2] :pos}]
  (-> primary-moon
      (update-in [:vel 0] + (compare px2 px1))
      (update-in [:vel 1] + (compare py2 py1))
      (update-in [:vel 2] + (compare pz2 pz1))))

(defn apply-velocity
  [{:keys [vel] :as moon}]
  (update moon :pos #(mapv + % vel)))

(defn abs [x] (if (< x 0) (- x) x))

(defn energy
  [coords]
  (reduce + (map abs coords)))

(defn potential-energy
  [moon]
  (energy (:pos moon)))

(defn kinetic-energy
  [moon]
  (energy (:vel moon)))

(defn total-energy
  [moon]
  (apply * ((juxt potential-energy kinetic-energy) moon)))

(defn simulate-step
  [moons]
  (map (comp apply-velocity #(reduce apply-gravity % moons)) moons))

(->> input
     lines->moons
     (iterate simulate-step)
     (drop 1000)
     first
     (map total-energy)
     (reduce +))

(defn simulate-counting-step
  [axis {:keys [moons seen step] :as state}]
  (let [axis-state (map #(vector (get (:pos %) axis)
                                 (get (:vel %) axis))
                        moons)]
    (if (seen axis-state)
      (assoc state :finished true)
      (-> state
          (update :moons simulate-step)
          (update :seen conj axis-state)
          (update :step inc)))))

(core/defn-split simulate-axis-to-loop
  [axis | moons]
  (let [axis-fn (partial simulate-counting-step axis)]
    (->> {:moons moons :seen #{} :step 0}
         (iterate axis-fn)
         (drop-while (complement :finished))
         first
         :step)))

#_(let [axis-fns (apply juxt (map simulate-axis-to-loop [0 1 2]))]
  (axis-fns (lines->moons input)))

;; This takes some time...
;; [84032 231614 193052]
;; LCM: 469671086427712
