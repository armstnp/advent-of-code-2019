(ns advent-of-code-2019.day5-sketch
  (:require [advent-of-code-2019.day5 :as soln]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

;; Pause and unpause the simulation by clicking or pressing any key

(def cells (count soln/input))
(def memory-cell-width 15)
(def memory-cell-height (q/ceil (/ cells memory-cell-width)))
(def cell-width 80)
(def cell-height 20)
(def mid-cell (/ cell-height 2))
(def memory-width (* cell-width memory-cell-width))
(def memory-height (* cell-height memory-cell-height))
(def diagnostic-height 20)
(def canvas-width memory-width)
(def canvas-height (+ diagnostic-height memory-height))
(def diagnostic-line (+ memory-height (/ diagnostic-height 2)))
(def diagnostic-offset (quot canvas-width 3))

(defn setup
  []
  (q/frame-rate 1)
  (q/color-mode :rgb)
  (q/background 0)
  (q/text-size 12)
  (q/text-align :left :center)
  (q/no-stroke)
  {:memory soln/input
   :ip 0
   :in-stack '(5)
   :out-queue []
   :paused true})

(defn addr-by-mode
  [mode value-addr value]
  (case mode
    0 value
    1 value-addr))

(def op-display
  {1 "+"
   2 "*"
   3 "in"
   4 "out"
   5 "j:t"
   6 "j:f"
   7 "<"
   8 "="
   99 "HALT"})

(defn draw-ip
  [ip touched?]
  (when touched?
    (q/fill 0 0 100)
    (q/rect 0 memory-height diagnostic-offset canvas-height))
  (q/fill 255 255 255)
  (q/text (str "IP: " ip) 0 diagnostic-line))

(defn draw-in-stack
  [[next-input] takes-input?]
  (when takes-input?
    (q/fill 0 50 0)
    (q/rect diagnostic-offset memory-height diagnostic-offset canvas-height))
  (q/fill 255 255 255)
  (q/text (str "NEXT IN: " next-input) diagnostic-offset diagnostic-line))

(defn draw-out-queue
  [out-queue emits-output?]
  (when emits-output?
    (q/fill 50 0 0)
    (q/rect (* 2 diagnostic-offset) memory-height (- canvas-width (* 2 diagnostic-offset)) canvas-height))
  (q/fill 255 255 255)
  (q/text (str "LAST OUT: " (first out-queue)) (* 2 diagnostic-offset) diagnostic-line))

(defn draw-state
  [{:keys [memory ip in-stack out-queue halt]}]
  (q/clear)
  (q/background (if halt 100 0) 0 0)
  (let [[opcode modes] (soln/parse-code (memory ip))
        inputs (case opcode
                 (1 2 5 6 7 8) (->> memory
                                    (drop (inc ip))
                                    (take 2)
                                    (map addr-by-mode modes (range (inc ip)))
                                    set)
                 4 (->> ip
                        inc
                        (nth memory)
                        (addr-by-mode (first modes) (inc ip))
                        hash-set)
                 (3 99) #{}
                 #{})
        output (case opcode
                 (1 2 7 8) (memory (+ ip 3))
                 3 (memory (inc ip))
                 (4 5 6 99) nil
                 nil)
        instr-range? (case opcode
                       (1 2 7 8) (set (range (inc ip) (+ ip 4)))
                       (5 6) (set (range (inc ip) (+ ip 3)))
                       (3 4) (hash-set (inc ip))
                       99 #{}
                       #{})
        takes-input? (= opcode 3)
        emits-output? (= opcode 4)
        touches-ip? (#{5 6} opcode)]
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
          (apply q/fill color)
          (q/rect x y cell-width cell-height)
          (q/fill 255 255 255)
          (q/text value-str (+ x 2) (+ y mid-cell))))
      memory))
    (draw-ip ip touches-ip?)
    (draw-in-stack in-stack takes-input?)
    (draw-out-queue out-queue emits-output?)))

(defn pause-unpause
  [state _event]
  (update state :paused not))

(q/defsketch day5-sketch
  :title "AoC 2019-5: Sunny with a Chance of Asteroids"
  :size [canvas-width canvas-height]

  :setup setup
  :update #(if (:paused %) % (soln/run-instr %))
  :draw draw-state
  :mouse-clicked pause-unpause
  :key-typed pause-unpause

  :middleware [m/fun-mode])

;; Hosted at http://quil.info/sketches/show/41aecab991e19a5ae696e9d6ade7cdd4fbc5463f250b777e7689a71cafa750eb
