(ns advent-of-code-2019.day15-sketch
  (:require [advent-of-code-2019.day15 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def start-pos [21 21])
(def starting-droid {:computer (soln/program->computer soln/input '())
                     :zone {start-pos soln/start}
                     :pos start-pos})

(def cells-wide 41)
(def cells-tall 41)
(def cell-size 25)
(def canvas-width (* cell-size cells-wide))
(def canvas-height (* cell-size cells-tall))
(def frames-per-tick 3)

(defn setup
  []
  (q/frame-rate 120)
  (q/color-mode :rgb)
  (q/background 0)
  (q/no-stroke)
  {:prev-state starting-droid
   :curr-state starting-droid
   :frame 0})

(defn update-state
  [{:keys [prev-state curr-state] :as state}]
  (let [{:keys [frame] :as state'} (update state :frame inc)]
    (if (zero? (mod frame frames-per-tick))
      (-> state'
          (assoc :prev-state curr-state)
          (update :curr-state soln/explore))
      state')))

(defn draw-aligned-cell
  [[x y]]
  (q/rect (* x cell-size) (* y cell-size) cell-size cell-size))

(defn draw-start
  []
  (q/fill 0 0 255)
  (draw-aligned-cell start-pos))

(defn draw-walls
  [{{:keys [zone]} :curr-state}]
  (q/fill 128 128 128)
  (doall
   (->> zone
        (filter #(= (second %) soln/wall))
        (map first)
        (map draw-aligned-cell))))

(defn draw-oxygen
  [{{:keys [oxygen]} :prev-state}]
  (when oxygen
    (q/fill 0 255 0)
    (draw-aligned-cell oxygen)))

(defn arrow-bounds
  [[x y]]
  (let [padding 6]
    {:x-min (+ (* cell-size x) padding)
     :x-max (dec (- (* cell-size (inc x)) padding))
     :y-min (+ (* cell-size y) padding)
     :y-max (dec (- (* cell-size (inc y)) padding))}))

(defn draw-north
  [{:keys [x-min x-max y-min y-max]}]
  (q/triangle (/ (+ x-min x-max) 2) y-min
              x-min y-max
              x-max y-max))

(defn draw-south
  [{:keys [x-min x-max y-min y-max]}]
  (q/triangle (/ (+ x-min x-max) 2) y-max
              x-min y-min
              x-max y-min))

(defn draw-west
  [{:keys [x-min x-max y-min y-max]}]
  (q/triangle x-min (/ (+ y-min y-max) 2)
              x-max y-min
              x-max y-max))

(defn draw-east
  [{:keys [x-min x-max y-min y-max]}]
  (q/triangle x-max (/ (+ y-min y-max) 2)
              x-min y-min
              x-min y-max))

(def cmds #{soln/cmd-move-north
            soln/cmd-move-south
            soln/cmd-move-west
            soln/cmd-move-east})

(def cmd->arrow
  {soln/cmd-move-north draw-north
   soln/cmd-move-south draw-south
   soln/cmd-move-west draw-west
   soln/cmd-move-east draw-east})

(defn draw-backtracks
  [{{:keys [zone]} :prev-state}]
  (q/fill 64 64 64)
  (doall
   (->> zone
        (filter (comp cmds second))
        (map (fn [[pos cmd]]
               ((cmd->arrow cmd)
                (arrow-bounds pos)))))))

(defn draw-droid
  [{{prev-pos :pos} :prev-state
    {curr-pos :pos} :curr-state}
   tick-frame]
  (let [move-pause 0
        max-frame (- frames-per-tick move-pause)
        interp (q/constrain (/ tick-frame max-frame) 0 1)
        interp-x (q/lerp (first prev-pos) (first curr-pos) interp)
        interp-y (q/lerp (second prev-pos) (second curr-pos) interp)]
    (q/fill 0 255 255)
    (q/rect (* cell-size interp-x) (* cell-size interp-y) cell-size cell-size)))

(defn draw-state
  [{:keys [frame] :as state}]
  (q/clear)
  (q/background 0)
  (let [tick-frame (mod frame frames-per-tick)]
    (draw-start)
    (draw-walls state)
    (draw-oxygen state)
    (draw-backtracks state)
    (draw-droid state tick-frame)))

(q/defsketch day15-sketch
  :title "AoC 2019-15: Oxygen System"
  :size [canvas-width canvas-height]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode])

;; Hosted at 


