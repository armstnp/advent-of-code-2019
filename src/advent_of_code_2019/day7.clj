(ns advent-of-code-2019.day7
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(def input (-> "day7.txt"
               core/read-input
               str/trim
               (str/split #",")
               (->> (mapv core/parse-int))))

;; Operations

(defn param-value
  [memory param-mode value]
  (case param-mode
    0 (memory value)
    1 value
    (str "Invalid param-mode " param-mode)))

(defn binary-op
  [{:keys [memory ip] :as computer} modes bin-fn]
  (let [[in-val-1 in-val-2] (map (partial param-value memory)
                                 modes
                                 (subvec memory (inc ip) (+ ip 3)))
        out-addr (nth memory (+ ip 3))
        out-val (bin-fn in-val-1 in-val-2)]
    (-> computer
        (assoc-in [:memory out-addr] out-val)
        (update :ip + 4))))

(defn op-add
  [computer modes]
  (binary-op computer modes +))

(defn op-mul
  [computer modes]
  (binary-op computer modes *))

(defn op-in
  [{:keys [memory ip in-stack] :as computer} _modes]
  (if (empty? in-stack)
    (assoc computer :awaiting-input true)
    (let [out-addr (nth memory (inc ip))
          in-val (first in-stack)]
      (-> computer
          (assoc-in [:memory out-addr] in-val)
          (update :ip + 2)
          (update :in-stack next)))))

(defn op-out
  [{:keys [memory ip out-queue] :as computer} [out-mode]]
  (let [out-val (->> ip inc (nth memory) (param-value memory out-mode))]
    (-> computer
        (update :out-queue conj out-val)
        (update :ip + 2))))

(defn jump-op
  [{:keys [memory ip] :as computer} modes test-fn instr-length]
  (let [[test-val jump-addr] (map (partial param-value memory)
                                  modes
                                  (subvec memory (inc ip) (+ ip 3)))
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
   99 op-halt})

(defn run-instr
  [{:keys [memory ip] :as computer}]
  (let [[opcode modes] (parse-code (memory ip))
        op-fn (opcode->fn opcode)]
    (op-fn computer modes)))

;; Program execution

(defn run-program
  [program input]
  (->> {:memory program :ip 0 :in-stack input :out-queue []}
       (iterate run-instr)
       (drop-while #(not (or (:halt %) (:awaiting-input %))))
       first))

(defn run-amplifiers
  [program initial-input [phase & phases]]
  (if phase
    (recur program
           (->> (list phase initial-input)
                (run-program program)
                :out-queue
                first)
           phases)
    initial-input))

(apply max (map #(run-amplifiers input 0 %) (combo/permutations '(0 1 2 3 4))))

(defn run-feedback-amplifiers
  [[computer & rest-computers :as computers] inputs]
  (if-not computer
    (first inputs)
    (let [computer' (-> computer
                        (dissoc :awaiting-input)
                        (update :in-stack concat inputs) ;; Add prior amp outputs to curr inputs
                        (->> (iterate run-instr)
                             (drop-while #(not (or (:halt %) (:awaiting-input %))))
                             first))
          outputs (:out-queue computer')
          computers' (cond
                       (:halt computer') ;; This machine's halted - remove it
                       rest-computers

                       (:awaiting-input computer') ;; Cycle machines to generate more input
                       (conj (vec rest-computers) (assoc computer' :out-queue [])))]
      (recur computers' outputs))))

(defn initial-amplifier
  [program phase]
  {:memory (mapv bigint program) :ip 0 :in-stack (list phase) :out-queue []})

(apply max (map #(run-feedback-amplifiers (map (partial initial-amplifier input) %) [0]) (combo/permutations '(5 6 7 8 9))))
