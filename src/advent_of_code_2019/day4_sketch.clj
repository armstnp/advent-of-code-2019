(ns advent-of-code-2019.day4-sketch
  (:require [advent-of-code-2019.day4 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def range-count (inc (- soln/input-max soln/input-min)))
(def canvas-size (-> range-count
                     Math/sqrt
                     Math/ceil
                     int))

(defn setup
  []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (q/background 0)
  (q/blend-mode :add)
  (q/no-fill)
  {:p1? false
   :p2? false
   :n (dec soln/input-min)})

(defn update-state-
  [{:keys [n]}]
  (let [n' (inc n)]
    {:p1? (soln/password-p1? n')
     :p2? (soln/password-p2? n')
     :n n'}))

(defn update-state
  [state]
  (let [state' (update-state- state)]
    (if ((some-fn :p1? :p2?) state')
      state'
      (recur state'))))

(defn draw-pixel
  [n r g b]
  (let [offset-n (- n soln/input-min)]
    (q/stroke r g b)
    (q/ellipse (mod offset-n canvas-size)
               (quot offset-n canvas-size)
               3 3)))

(defn draw-state
  [{:keys [p1? p2? n]}]
  (cond
    p2? (draw-pixel n 0 100 0)
    p1? (draw-pixel n 100 0 0)))

(q/defsketch day4-sketch
  :title "AoC 2019-4: Secure Container"
  :size [canvas-size canvas-size]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode])

;; Hosted at http://quil.info/sketches/show/4350261fceb6f0e6f385e23fcd9a8ca91623ffd813912ca79c47bfa51ad1a6f1
