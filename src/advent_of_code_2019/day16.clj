;; Set NS before starting: C-c M-n n
(ns advent-of-code-2019.day16
  (:require [advent-of-code-2019.core :as core]
            [advent-of-code-2019.left-shark :as ls]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [is-prime.core :refer [is-prime]]))
;;  (:import  [org.jgrapht.graph SimpleDirectedWeightedGraph Pseudograph DefaultEdge DefaultWeightedEdge]))

(def input (->> "day16.txt" core/read-input str/split-lines))

(def parse-line
  (comp
    :components
    (ls/parse
     ;; Fill in parse components here
     )))
