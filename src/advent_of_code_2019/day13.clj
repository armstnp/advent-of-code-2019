(ns advent-of-code-2019.day13
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [quil.core :as q :include-macros true]
            [quil.middleware :as m]))

(def input (-> "day13.txt"
               core/read-input
               str/trim
               (str/split #",")
               (->> (mapv bigint))))

;; Operations

(defn mem-get
  [memory n]
  (if (>= n (count memory)) 0 (memory n)))

(defn mem-get-range
  [memory start size]
  (let [end (+ start size)
        m-size (count memory)]
    (if (> end m-size)
      (into (subvec memory start m-size) (repeat (- end m-size) 0))
      (subvec memory start end))))

(defn mem-set
  [memory n value]
  (let [size (count memory)
        memory' (if (>= n size)
                  (into memory (repeat (inc (- n size)) 0))
                  memory)]
    (assoc memory' n value)))

(defn in-param-value
  [{:keys [memory rel-base]} param-mode value]
  (case param-mode
    0 (mem-get memory value)
    1 value
    2 (mem-get memory (+ value rel-base))
    (str "Invalid param-mode " param-mode)))

(defn out-param-addr
  [{:keys [memory rel-base]} param-mode addr]
  (+ (mem-get memory addr)
     (case param-mode
       0 0
       2 rel-base
       (str "Invalid param-mode " param-mode))))

(defn binary-op
  [{:keys [memory ip] :as computer} [in-mode-1 in-mode-2 out-mode] bin-fn]
  (let [[in-val-1 in-val-2] (map (partial in-param-value computer)
                                 [in-mode-1 in-mode-2]
                                 (mem-get-range memory (inc ip) 2))
        out-addr (out-param-addr computer out-mode (+ ip 3))
        out-val (bin-fn in-val-1 in-val-2)]
    (-> computer
        (update :memory #(mem-set % out-addr out-val))
        (update :ip + 4))))

(defn op-add
  [computer modes]
  (binary-op computer modes +))

(defn op-mul
  [computer modes]
  (binary-op computer modes *))

(defn op-in
  [{:keys [memory ip in-stack] :as computer} [out-mode]]
  (if (empty? in-stack)
    (assoc computer :awaiting-input true)
    (let [out-addr (out-param-addr computer out-mode (inc ip))
          in-val (first in-stack)]
      (-> computer
          (update :memory #(mem-set % out-addr in-val))
          (update :ip + 2)
          (update :in-stack next)))))

(defn op-out
  [{:keys [memory ip out-queue] :as computer} [out-mode]]
  (let [out-val (->> ip inc (mem-get memory) (in-param-value computer out-mode))]
    (-> computer
        (update :out-queue conj out-val)
        (update :ip + 2))))

(defn jump-op
  [{:keys [memory ip] :as computer} modes test-fn instr-length]
  (let [[test-val jump-addr] (map (partial in-param-value computer)
                                  modes
                                  (mem-get-range memory (inc ip) 2))
        ip' (if (test-fn test-val)
              jump-addr
              (+ ip instr-length))]
    (assoc computer :ip ip')))

(defn op-jump-if-true
  [computer modes]
  (jump-op computer modes (complement zero?) 3))

(defn op-jump-if-false
  [computer modes]
  (jump-op computer modes zero? 3))

(defn op-less-than
  [computer modes]
  (binary-op computer modes #(if (< %1 %2) 1 0)))

(defn op-equals
  [computer modes]
  (binary-op computer modes #(if (= %1 %2) 1 0)))

(defn op-adjust-rel-base
  [{:keys [memory ip] :as computer} [in-mode]]
  (let [value (->> ip
                   inc
                   (mem-get memory)
                   (in-param-value computer in-mode))]
    (-> computer
        (update :rel-base + value)
        (update :ip + 2))))

(defn op-halt
  [computer _modes]
  (assoc computer :halt true))

;; Instruction parsing and handling

(defn parse-opcode
  [code]
  (rem code 100))

(defn parse-modes
  "Lazy, infinite seq of modes; handle with care"
  [code]
  (->> 100
       (quot code)
       (iterate #(quot % 10))
       (map #(rem % 10))))

(def parse-code (juxt parse-opcode parse-modes))

(def opcode->fn
  {1  op-add
   2  op-mul
   3  op-in
   4  op-out
   5  op-jump-if-true
   6  op-jump-if-false
   7  op-less-than
   8  op-equals
   9  op-adjust-rel-base
   99 op-halt})

(defn run-instr
  [{:keys [memory ip] :as computer}]
  (let [[opcode modes] (parse-code (mem-get memory ip))
        op-fn (opcode->fn opcode)]
    (op-fn computer modes)))

;; Program execution

(defn init-program
  [program input]
  {:memory program :ip 0 :rel-base 0 :in-stack input :out-queue []})

(defn run-program
  [program]
  (->> program
       (iterate run-instr)
       (drop-while #(not (or (:halt %) (:awaiting-input %))))
       first))

(def screen (-> input
                (init-program '())
                run-program
                :out-queue))

(->> screen
     (drop 2)
     (take-nth 3)
     (filter (partial = 2))
     count)

;; Drawing

(let [[min-x max-x] (->> screen
                         (partition 3)
                         (map first)
                         ((juxt (partial apply min) (partial apply max))))
      [min-y max-y] (->> screen
                         (partition 3)
                         (map second)
                         ((juxt (partial apply min) (partial apply max))))]
  (def min-x min-x)
  (def max-x max-x)
  (def min-y min-y)
  (def max-y max-y))

(def cell-size 30)
(def score-buffer 20)
(def canvas-width (* cell-size (inc max-x)))
(def canvas-height (+ (* cell-size (inc max-y)) score-buffer))


(defn setup
  []
  (q/frame-rate 60)
  (q/color-mode :rgb)
  (q/background 0)
  (q/text-size 10)
  (q/text-align :left :center)
  (q/no-stroke)
  {:memory (assoc input 0 2)
   :ip 0
   :rel-base 0
   :in-stack '()
   :out-queue []})

(defn draw-score
  [score win?]
  (q/fill 0 0 0)
  (q/rect 0 (- canvas-height score-buffer) canvas-width score-buffer)
  (q/fill 255 255 255)
  (q/text (str "Score: " score) 1 (- canvas-height (/ score-buffer 2)))
  (when win?
    (do
      (q/text-align :right :center)
      (q/text "You Win!" (dec canvas-width) (- canvas-height (/ score-buffer 2)))
      (q/text-align :left :center))))

(defn draw-tile
  [x y tile]
  (apply q/fill (case tile
                  0 [0     0   0 200]
                  1 [128 128 128 100]
                  2 [  0   0 255 100]
                  3 [  0 255   0 100]
                  4 [255   0   0 100]))
  (if (= tile 4)
    (q/ellipse (+ (/ cell-size 2) (* cell-size x))
               (+ (/ cell-size 2) (* cell-size y))
               cell-size
               cell-size)
    (q/rect (* cell-size x) (* cell-size y) cell-size cell-size)))

(defn draw-element
  [halt? [x y tile]]
  (if (and (= x -1) (zero? y))
    (draw-score tile halt?)
    (draw-tile x y tile)))

(defn draw-state
  [{:keys [out-queue halt] :as computer}]
  (doall
   (map (partial draw-element halt) (partition 3 out-queue))))

(defn seek-tile
  [raw-output tile]
  (->> raw-output
       (partition 3)
       (filter #(= tile (nth % 2)))
       first
       first))

(defn update-state
  [{:keys [in-stack out-queue halt ball-x paddle-x] :as computer}]
  (if halt
    computer
    (let [ball-x' (or (seek-tile out-queue 4) ball-x)
          paddle-x' (or (seek-tile out-queue 3) paddle-x)
          input (cond
                  (nil? ball-x') '(0)
                  (nil? paddle-x') '(0)
                  (< ball-x' paddle-x') '(-1)
                  (> ball-x' paddle-x') '(1)
                  :else '(0))]
      (-> computer
          (assoc :in-stack input)
          (dissoc :awaiting-input)
          (assoc :out-queue [])
          (assoc :ball-x ball-x')
          (assoc :paddle-x paddle-x')
          run-program))))

(q/defsketch day13-sketch
  :title "AoC 2019-13: Care Package"
  :size [canvas-width canvas-height]

  :setup setup
  :update update-state
  :draw draw-state

  :middleware [m/fun-mode])

;; Hosted at http://quil.info/sketches/show/f35ebdfb776bb34d65a82c6d6599b4f3991d93dc80a880123f929e2cda836a64
