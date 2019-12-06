(ns advent-of-code-2019.day6
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str])
  (:import  [org.jgrapht.graph DirectedAcyclicGraph DefaultEdge]
            [org.jgrapht.alg.lca NaiveLCAFinder]))

(def input (->> "day6.txt" core/read-input str/split-lines (map #(str/split % #"\)"))))

(defn vertices
  [edges]
  (set (mapcat identity edges)))

(defn edges->dag
  [edges]
  (let [dag (DirectedAcyclicGraph. DefaultEdge)]
    (do
      (doall (map #(.addVertex dag %) (vertices input)))
      (doall (map (fn [[src dest]] (.addEdge dag src dest)) input))
      dag)))

(defn count-ancestors-fn
  [dag]
  (comp count #(.getAncestors dag %)))

(let [dag (edges->dag input)]
  (->> dag
       .iterator
       iterator-seq
       (map (count-ancestors-fn dag))
       (reduce +)))

(let [dag (edges->dag input)
      count-ancestors (count-ancestors-fn dag)
      lca-finder (NaiveLCAFinder. dag)
      lca (.getLCA lca-finder "YOU" "SAN")
      you-ancestors (count-ancestors "YOU")
      santa-ancestors (count-ancestors "SAN")
      lca-ancestors (count-ancestors lca)
      steps-up (- you-ancestors lca-ancestors 1)
      steps-down (- santa-ancestors lca-ancestors 1)]
  (+ steps-up steps-down))
