(ns advent-of-code-2019.day1-sketch
  (:require [advent-of-code-2019.day1 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(defn setup
  []
  (q/frame-rate 2)
  (q/color-mode :hsb)
  (q/background 0)
  (map vector soln/input))

(defn update-state
  [mass-series]
  (map (fn [masses]
         (->> masses
              last
              soln/fuel-for-mass
              (conj masses)
              (filterv #(> % 0))))
       mass-series))

(def h-width 5)
(def v-scale #(q/ceil (* 100 (q/log %))))
(def canvas-height (->> soln/input
                        (apply max)
                        v-scale
                        int))
(def canvas-width (* h-width (count soln/input)))

(def num-colors 10)

(defn color-for-index
  [i]
  (quot (* i 255) num-colors))

(defn set-color-for-index
  [i]
  (q/fill (color-for-index i) 200 (- 255 (color-for-index i)))
  (q/no-stroke))

(defn draw-bar-rect
  [bar-i rect-i value]
  (set-color-for-index rect-i)
  (let [bar-height (v-scale value)]
    (q/rect (* bar-i h-width)
            (- canvas-height bar-height)
            h-width
            bar-height)))

(defn draw-state
  [mass-series]
  (q/background 0)
  (doall
   (map-indexed (fn [bar-i masses]
                  (doall
                   (map-indexed (partial draw-bar-rect bar-i) masses)))
                mass-series)))

(q/defsketch cluster
  :title "AoC 2019-1: The Tyranny of the Rocket Equation"
  :size [canvas-width canvas-height]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode])

;; Hosted at http://www.quil.info/sketches/show/7bf3d4b9aa56cde37448026ed523f38bf6b81dce2bd4c9433fa4f5f584e0570b
