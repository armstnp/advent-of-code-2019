(ns advent-of-code-2019.day3-sketch
  (:require [advent-of-code-2019.day3 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def canvas-size 1000)

(def wire-raw-segments (map soln/parse-wire soln/input))
(def lines (map soln/wire-lines wire-raw-segments))
(def all-corners (->> lines
                      (mapcat identity)
                      (mapcat identity)
                      set))
(def min-x (apply min (map first all-corners)))
(def max-x (apply max (map first all-corners)))
(def min-y (apply min (map second all-corners)))
(def max-y (apply max (map second all-corners)))
(def x-range (inc (- max-x min-x)))
(def y-range (inc (- max-y min-y)))
(def scale-factor (/ canvas-size (max x-range y-range)))
(def x-pad (/ (- canvas-size (* scale-factor x-range)) 2))
(def y-pad (/ (- canvas-size (* scale-factor y-range)) 2))
(def map-x #(+ x-pad (* scale-factor (- % min-x))))
(def map-y #(+ y-pad canvas-size (* scale-factor (- min-y %))))
(defn map-pos
  [[x y]]
  [(map-x x) (map-y y)])

(def collisions
  (set
   (mapcat
    identity
    (for [segment-1 (first lines)
          segment-2 (second lines)
          :when (not (and (= (first segment-1) [0 0])
                          (= (first segment-2) [0 0])))]
      (soln/collide segment-1 segment-2)))))

(defn setup
  []
  (q/frame-rate 100)
  (q/color-mode :rgb)
  (q/background 0)
  (q/stroke-weight 1)
  [{:wire (first wire-raw-segments)
    :intersections collisions
    :pos [0 0]
    :found {}
    :step-i 1}
   {:wire (second wire-raw-segments)
    :intersections collisions
    :pos [0 0]
    :found {}
    :step-i 1}])

(def update-100-times (apply comp (repeat 100 soln/step-wire-current)))
(def update-state (partial mapv update-100-times))

(defn draw-lines
  [lines]
  (doall
   (map (fn [line]
          (->> line
               (mapcat map-pos)
               (apply q/line)))
        lines)))

(defn draw-point
  [[x y]]
  (q/ellipse (map-x x) (map-y y) 6 6))

(defn draw-state
  [state]
  (q/clear)

  ;; Draw the wires
  (q/no-fill)
  (q/stroke 0 255 255)
  (draw-lines (first lines))
  (q/stroke 255 255 0)
  (draw-lines (second lines))

  ;; Draw the center
  (q/no-stroke)
  (q/fill 255 255 255)
  (draw-point [0 0])

  ;; Draw the collisions
  (q/fill 255 0 0)
  (doall (map draw-point collisions))

  ;; Draw the traces
  (q/fill 0 255 0)
  (doall (map (comp draw-point :pos) state)))

(q/defsketch day3-sketch
  :title "AoC 2019-3: Crossed Wires"
  :size [(inc canvas-size) (inc canvas-size)]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode m/pause-on-error])

;; Hosted at http://quil.info/sketches/show/3045c99a9e8afbecc5e229977f6a828c0747e3073eb54b45111cd5e3717aa7db
