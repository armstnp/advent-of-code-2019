(ns advent-of-code-2019.day3
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(defn parse-wire
  [line]
  (-> line
      (str/split #",")
      (->> (map (fn [segment]
                  {:dir (first segment)
                   :length (core/parse-int (subs segment 1))})))))

(def input (->> "day3.txt" core/read-input str/split-lines))

(defn move-by-segment
  [[x y :as pos] {:keys [dir length]}]
  (case dir
    \R (update pos 0 + length)
    \L (update pos 0 - length)
    \U (update pos 1 + length)
    \D (update pos 1 - length)
    dir))

(defn wire-lines
  [segments]
  (->> segments
       (reductions move-by-segment [0 0])
       (partition 2 1)))

(defn horiz?
  [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn vert?
  [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn horiz-y
  [[[_ y]]]
  y)

(defn vert-x
  [[[x _]]]
  x)

(defn in-range?
  [min-x max-x x]
  (and (>= x min-x) (<= x max-x)))

(defn collide-hv
  [[[x1 _] [x2 _] :as h-wire]
   [[_ y1] [_ y2] :as v-wire]]
  (let [x (vert-x v-wire)
        y (horiz-y h-wire)
        min-x (min x1 x2)
        max-x (max x1 x2)
        min-y (min y1 y2)
        max-y (max y1 y2)]
    (when (and (in-range? min-x max-x x)
               (in-range? min-y max-y y))
      [[x y]])))

(defn collide
  [wire-1 wire-2]
  (cond
    (and (horiz? wire-1) (vert? wire-2)) (collide-hv wire-1 wire-2)
    (and (horiz? wire-2) (vert? wire-1)) (collide-hv wire-2 wire-1)
    :else [])) ;; Assuming no horiz / vert wires overlap

(let [[wire-1 wire-2] (map (comp wire-lines parse-wire) input)
      collisions (for [segment-1 wire-1
                       segment-2 wire-2
                       :when (not (and (= (first segment-1) [0 0])
                                       (= (first segment-2) [0 0])))]
                   (collide segment-1 segment-2))]
  (->> collisions
       (mapcat identity)
       (map (comp (partial apply +) (partial map #(if (< % 0) (- %) %))))
       sort
       first))

(defn step-wire-current
  [{:keys [wire intersections pos found step-i] :as step}]
  (if (empty? wire)
    step
    (let [{:keys [dir length] :as segment} (first wire)
          wire' (if (= length 1)
                  (next wire)
                  (conj (next wire) (update segment :length dec)))
          pos' (move-by-segment pos {:dir dir :length 1})
          found' (if (and (intersections pos') (not (found pos')))
                   (assoc found pos' step-i)
                   found)
          step-i' (inc step-i)]
      (assoc step
             :wire wire'
             :pos pos'
             :found found'
             :step-i step-i'))))

(defn all-intersections-found?
  [[{:keys [intersections] found-a :found}
    {found-b :found}]]
   (= (count intersections) (count found-a) (count found-b)))

(defn find-earliest-intersection
  [input]
  (let [wire-raw-segments (map parse-wire input)
        [wire-1 wire-2] (map wire-lines wire-raw-segments)
        collisions (for [segment-1 wire-1
                         segment-2 wire-2
                         :when (not (and (= (first segment-1) [0 0])
                                         (= (first segment-2) [0 0])))]
                     (collide segment-1 segment-2))
        collisions (->> collisions (mapcat identity) set)]
    (->> [{:wire (first wire-raw-segments)
           :intersections collisions
           :pos [0 0]
           :found {}
           :step-i 1}
          {:wire (second wire-raw-segments)
           :intersections collisions
           :pos [0 0]
           :found {}
           :step-i 1}]
         (iterate (partial mapv step-wire-current))
         (drop-while (complement all-intersections-found?))
         first
         (map :found)
         (apply merge-with +)
         vals
         sort
         first)))

(find-earliest-intersection input)
