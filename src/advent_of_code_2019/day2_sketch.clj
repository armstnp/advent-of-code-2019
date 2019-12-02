(ns advent-of-code-2019.day2-sketch
  (:require [advent-of-code-2019.day2 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def cells (count soln/input))
(def canvas-cell-width (q/ceil (q/sqrt cells)))
(def canvas-cell-height (q/ceil (/ cells canvas-cell-width)))
(def cell-width 80)
(def cell-height 20)
(def canvas-width (* cell-width canvas-cell-width))
(def canvas-height (* cell-height canvas-cell-height))

(defn setup
  []
  (q/frame-rate 1)
  (q/color-mode :rgb)
  (q/background 0)
  (q/text-size 12)
  (q/text-align :left :top)
  (q/no-stroke)
  {:memory (-> soln/input (assoc 1 49) (assoc 2 67)) :ip 0})

(defn draw-state
  [{:keys [memory ip halt]}]
  (q/clear)
  (q/background (if halt 100 0) 0 0)
  (let [op (memory ip)
        inputs (case op
                 (1 2) (->> memory (drop (inc ip)) (take 2) set)
                 99 #{}
                 #{})
        output (case op
                 (1 2) (memory (+ ip 3))
                 99 nil
                 nil)
        instr-range? (case op
                       (1 2) (set (range (inc ip) (+ ip 4)))
                       99 #{}
                       #{})]
    (doall
     (map-indexed
      (fn [addr value]
        (let [x (* cell-width (mod addr canvas-cell-width))
              y (* cell-height (quot addr canvas-cell-width))
              op? (= addr ip)
              input? (inputs addr)
              output? (= output addr)
              red (if op? 50 0)
              green (if input? 50 0)
              blue (if output? 100 0)
              color (map +
                         (if halt [100 0 0] [red green blue])
                         (if (instr-range? addr) [100 100 100] [0 0 0]))
              value-str (if op?
                          (str value " (" (case op 1 "+" 2 "*" 99 "HALT" "???") ")")
                          (str value))]
          (apply q/fill color)
          (q/rect x y cell-width cell-height)
          (q/fill 255 255 255)
          (q/text value-str x y)))
      memory))))

(q/defsketch day2-sketch
  :title "AoC 2019-2: 1202 Program Alarm"
  :size [canvas-width canvas-height]

  :setup setup
  :update soln/run-instr
  :draw draw-state

  :middleware [m/fun-mode])
