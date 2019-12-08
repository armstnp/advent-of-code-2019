(ns advent-of-code-2019.day8-sketch
  (:require [advent-of-code-2019.day8 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def scale 20)
(def border 4)
(def canvas-width (+ (* 2 border) (* soln/width scale)))
(def canvas-height (+ (* 2 border) (* soln/height scale)))
(def layers (->> soln/input (partition soln/layer-area) reverse))
(def tick-reset 20)

(defn setup
  []
  (q/frame-rate 120)
  (q/color-mode :rgb 16)
  (q/background 0)
  (q/fill 8)
  (q/rect border border (* soln/width scale) (* soln/height scale))
  (q/no-stroke)
  {:tick tick-reset
   :current (repeat soln/layer-area 8)
   :target (repeat soln/layer-area 8)
   :next-layer 0})

(defn mix-layers
  [layer-current layer-on-top]
  (map #(case %2
          \0 0
          \1 16
          \2 %1)
       layer-current
       layer-on-top))

(defn blend-towards-target
  [current target]
  (map #(cond
          (< %1 %2) (inc %1)
          (> %1 %2) (dec %1)
          :else %1)
       current
       target))

(defn update-state
  [{:keys [current target tick next-layer]}]
  (let [next? (and (zero? tick) (< next-layer (count layers)))
        tick' (if next? tick-reset (dec tick))
        target' (if next? (mix-layers target (nth layers next-layer)) target)
        next-layer' (if next? (inc next-layer) next-layer)
        current' (blend-towards-target current target')]
    {:tick tick'
     :current current'
     :target target'
     :next-layer next-layer'}))

(defn draw-pixel
  [index color]
  (if-not (= color \2)
    (let [x (->> soln/width
                 (mod index)
                 (* scale)
                 (+ border))
          y (->> soln/width
                 (quot index)
                 (* scale)
                 (+ border))]
      (q/fill color)
      (q/rect x y scale scale))))

(defn draw-state
  [{:keys [current]}]
  (doall (map-indexed draw-pixel current)))

(q/defsketch day8-sketch
  :title "AoC 2019-8: Space Image Format"
  :size [canvas-width canvas-height]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode])

;; Hosted at http://quil.info/sketches/show/66648043ce20f4ae77390dbc6c628f743e8540e33fcb5535f4305c473fc8d30c
