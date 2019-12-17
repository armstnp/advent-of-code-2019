(ns advent-of-code-2019.day17
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (-> "day17.txt"
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

(defn interrupted?
  [{:keys [halt awaiting-input]}]
  (or halt awaiting-input))

;; Program execution

(defn program->computer
  [program input]
  {:memory program
   :ip 0
   :rel-base 0
   :in-stack input
   :out-queue []})

(defn run-computer-to-interrupt
  [computer]
  (->> computer
       (iterate run-instr)
       (drop-while (complement interrupted?))
       first))

(defn computer-io
  "Emits [computer' output]"
  [computer input]
  (let [computer' (-> computer
                      (assoc :in-stack (list input))
                      (dissoc :awaiting-input)
                      run-computer-to-interrupt)
        output (-> computer' :out-queue first)]
    [(assoc computer' :out-queue []) output]))

;; Solving the problem

(core/defn-split self-and-neighbors-fn
  [zone | pos]
  (->> [[ 0  0]
        [-1  0]
        [ 1  0]
        [ 0 -1]
        [ 0  1]]
       (map (partial map + pos))
       (map (partial get-in zone))
       (filter (complement nil?))))

(let [zone (-> input
               (program->computer '())
               run-computer-to-interrupt
               :out-queue
               (->>
                (map char)
                (apply str)
                str/split-lines
                (mapv vec)))
      height (count zone)
      width (count (first zone))
      self-and-neighbors (self-and-neighbors-fn zone)]
  (reduce +
          (for [col (range width)
                row (range height)
                :let [neighbors (self-and-neighbors [row col])]
                :when (every? (partial = \#) neighbors)]
            (* row col))))

(def awake-program (assoc input 0 2N))
(def movement (map int "A,B,A,B,C,B,C,A,C,C\nR,12,L,10,L,10\nL,6,L,12,R,12,L,4\nL,12,R,12,L,6\nn\n"))
(-> awake-program
    (program->computer movement)
    run-computer-to-interrupt
    :out-queue
    last)
