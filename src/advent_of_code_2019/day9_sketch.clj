(ns advent-of-code-2019.day9-sketch
  (:require [advent-of-code-2019.day9 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;; Pause and unpause the simulation by clicking or pressing any key

(def cells 1077)
(def memory-cell-width 37)
(def memory-cell-height (q/ceil (/ cells memory-cell-width)))
(def cell-width 24)
(def cell-height 24)
(def mid-cell (/ cell-height 2))
(def memory-width (* cell-width memory-cell-width))
(def memory-height (* cell-height memory-cell-height))
(def diagnostic-height 20)
(def canvas-width memory-width)
(def canvas-height (+ diagnostic-height memory-height))
(def diagnostic-line (+ memory-height (/ diagnostic-height 2)))
(def diagnostic-offset (quot canvas-width 4))

(defn setup
  []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (q/background 0)
  (q/text-size 10)
  (q/text-align :left :center)
  (q/no-stroke)
  {:memory (into soln/input (repeat (- cells (count soln/input)) 0))
   :ip 0
   :rel-base 0
   :in-stack '(2)
   :out-queue []
   :paused true})

(defn addr-by-mode
  [rel-base mode value-addr value]
  (case mode
    0 value
    1 value-addr
    2 (+ value rel-base)))

(def op-display
  {1 "+"
   2 "*"
   3 "in"
   4 "out"
   5 "j:t"
   6 "j:f"
   7 "<"
   8 "="
   9 "rel~"
   99 "HALT"})

(defn draw-ip
  [ip touched?]
  (when touched?
    (q/fill 0 0 100)
    (q/rect 0 memory-height diagnostic-offset canvas-height))
  (q/fill 255 255 255)
  (q/text (str "IP: " ip) 0 diagnostic-line))

(defn draw-rel-base
  [rel-base touched?]
  (when touched?
    (q/fill 0 0 100)
    (q/rect diagnostic-offset memory-height diagnostic-offset canvas-height))
  (q/fill 255 255 255)
  (q/text (str "Rel-Base: " rel-base) diagnostic-offset diagnostic-line))

(defn draw-in-stack
  [[next-input] takes-input?]
  (let [x (* 2 diagnostic-offset)]
    (when takes-input?
      (q/fill 0 50 0)
      (q/rect x memory-height diagnostic-offset canvas-height))
    (q/fill 255 255 255)
    (q/text (str "NEXT IN: " next-input) x diagnostic-line)))

(defn draw-out-queue
  [out-queue emits-output?]
  (let [x (* 3 diagnostic-offset)]
    (when emits-output?
      (q/fill 50 0 0)
      (q/rect x memory-height (- canvas-width x) canvas-height))
    (q/fill 255 255 255)
    (q/text (str "LAST OUT: " (first out-queue)) x diagnostic-line)))

(defn draw-state
  [{:keys [memory ip rel-base in-stack out-queue halt] :as computer}]
  (let [[opcode modes] (soln/parse-code (soln/mem-get memory ip))
        inputs (case opcode
                 (1 2 5 6 7 8) (->> (soln/mem-get-range memory (inc ip) 2)
                                    (map (partial addr-by-mode rel-base)
                                         modes
                                         (range (inc ip) (+ 3 ip)))
                                    set)
                 (4 9) (->> ip
                            inc
                            (soln/mem-get memory)
                            (addr-by-mode rel-base (first modes) (inc ip))
                            hash-set)
                 (3 99) #{}
                 #{})
        output (case opcode
                 (1 2 7 8) (soln/out-param-addr computer (nth modes 2) (+ ip 3))
                 3 (soln/out-param-addr computer (first modes) (inc ip))
                 (4 5 6 9 99) nil
                 nil)
        instr-range? (case opcode
                       (1 2 7 8) (set (range (inc ip) (+ ip 4)))
                       (5 6) (set (range (inc ip) (+ ip 3)))
                       (3 4 9) (hash-set (inc ip))
                       99 #{}
                       #{})
        takes-input? (= opcode 3)
        emits-output? (= opcode 4)
        touches-ip? (#{5 6} opcode)
        touches-rel-base? (= opcode 9)]
    (doall
     (map-indexed
      (fn [addr value]
        (let [x (* cell-width (mod addr memory-cell-width))
              y (* cell-height (quot addr memory-cell-width))
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
                          (str value " (" (op-display opcode) ")")
                          (str value))]
          (if (or op? input? output? (instr-range? addr))
            (do
              (apply q/fill color)
              (q/rect x y cell-width cell-height))
            (do
              (q/fill 0 0 0 30)
              (q/rect x y cell-width cell-height)))))
      memory))
    (q/fill 0 0 0 255)
    (q/rect 0 memory-height canvas-width (- canvas-height memory-height))
    (draw-ip ip touches-ip?)
    (draw-rel-base rel-base touches-rel-base?)
    (draw-in-stack in-stack takes-input?)
    (draw-out-queue out-queue emits-output?)))

(defn pause-unpause
  [state _event]
  (update state :paused not))

(q/defsketch day9-sketch
  :title "AoC 2019-9: Sensor Boost"
  :size [canvas-width canvas-height]

  :setup setup
  :update #(if (:paused %) % (soln/run-instr %))
  :draw draw-state
  :mouse-clicked pause-unpause
  :key-typed pause-unpause

  :middleware [m/fun-mode])

;; Hosted at http://quil.info/sketches/show/a86e31254e4e2b7275aa6363279390b7d369d44b0dfd5d6da33f8039e9163fb9
