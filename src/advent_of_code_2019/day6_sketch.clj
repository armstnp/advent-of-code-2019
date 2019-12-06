(ns advent-of-code-2019.day6-sketch
  (:require [advent-of-code-2019.core :as core]
            [advent-of-code-2019.day6 :as soln]
            [clojure.string :as str]
            [clojure.set :as s])
  (:import  [org.jgrapht.graph DirectedAcyclicGraph DefaultEdge]
            [org.jgrapht.alg.lca NaiveLCAFinder]))

(core/defn-split edge->dot-fn
  [dag path-vertices | edge]
  (let [source (.getEdgeSource dag edge)
        target (.getEdgeTarget dag edge)
        base-edge (str "  \"" source "\" -> \"" target "\"")
        pen-info (when (and (path-vertices source)
                            (path-vertices target))
                   "[color=red,penwidth=3.0]")]
    (str base-edge pen-info ";")))

(let [dag (soln/edges->dag soln/input)
      lca-finder (NaiveLCAFinder. dag)
      lca (.getLCA lca-finder "YOU" "SAN")
      you-ancestors (set (.getAncestors dag "YOU"))
      santa-ancestors (set (.getAncestors dag "SAN"))
      lca-ancestors (set (.getAncestors dag lca))
      touched-vertices (s/union
                        (s/difference you-ancestors lca-ancestors)
                        (s/difference santa-ancestors lca-ancestors)
                        #{"YOU" "SAN"})
      edge->dot (edge->dot-fn dag touched-vertices)
      edges (-> dag
                .edgeSet
                .iterator
                iterator-seq)]
  (println (str "digraph {\n"
       (str/join "\n" (map edge->dot edges))
       "\n}")))
