(ns advent-of-code-2019.day10-sketch
  (:require [advent-of-code-2019.day10 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def cell-width 9)
(def canvas-width (* cell-width (soln/width soln/input)))
(def canvas-height (* cell-width (soln/height soln/input)))

(defn center
  [cell]
  (map #(+ (/ (dec cell-width) 2) (* % cell-width)) cell))

(defn setup
  []
  (q/frame-rate 10)
  (q/color-mode :rgb)
  (q/background 0)
  {:asteroids (set (soln/map->asteroids soln/input))
   :lines (soln/polar-lines soln/input soln/station)})

(defn update-state
  [{:keys [asteroids lines] :as state}]
  (let [[[next-asteroid & rest-asteroids] & rest-lines] lines
        line' (if (empty? rest-asteroids) [] [rest-asteroids])
        lines' (concat rest-lines line')]
    (-> state
        (update :asteroids disj next-asteroid)
        (assoc :lines lines'))))

(defn draw-asteroids
  [asteroids]
  (q/no-stroke)
  (q/fill 0 100 200)
  (doall
   (map (comp #(q/ellipse (first %) (second %) 3 3) center) asteroids)))

(defn draw-laser
  [[[next-asteroid]]]
  (when next-asteroid
    (q/no-fill)
    (q/stroke 255 0 0)
    (q/line (center next-asteroid) (center soln/station))))

(defn draw-state
  [{:keys [asteroids lines]}]
  (q/clear)
  (q/background 0)
  (draw-asteroids asteroids)
  (draw-laser lines))

(q/defsketch day10-sketch
  :title "AoC 2019-10: Monitoring Station"
  :size [canvas-width canvas-height]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode])

;; Hosted at http://quil.info/sketches/show/5d471cd687426dd3ffa006d6c50133e7aed9c984ccada11be36ea06ab5fc5970

