(ns advent-of-code-2019.day15
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (-> "day15.txt"
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

;; Droid Mechanics

(def cmd-move-north 1)
(def cmd-move-south 2)
(def cmd-move-west 3)
(def cmd-move-east 4)

(def cmd-reverse
  {cmd-move-north cmd-move-south
   cmd-move-south cmd-move-north
   cmd-move-east cmd-move-west
   cmd-move-west cmd-move-east})

(def move-offset
  {cmd-move-north [ 0 -1]
   cmd-move-south [ 0  1]
   cmd-move-west  [-1  0]
   cmd-move-east  [ 1  0]})

(defn move-by-cmd
  [pos cmd]
  (mapv + pos (move-offset cmd)))

(def wall \#)
(def start \*)
(def oxygen \!)

(defn process-feedback
  [droid cmd target-pos feedback]
  (case feedback
    0 (assoc-in droid [:zone target-pos] wall)
    1 (-> droid
          (assoc-in [:zone target-pos] (cmd-reverse cmd))
          (assoc :pos target-pos))
    2 (-> droid
          (assoc-in [:zone target-pos] (cmd-reverse cmd))
          (assoc :pos target-pos)
          (assoc :oxygen target-pos))
    (println feedback)))

(def init-zone {[0 0] start})

(defn init-droid
  [program]
  {:zone init-zone
   :pos [0 0]
   :computer (program->computer input '())})

(defn neighbors
  "[[neighbor-pos cmd]]"
  [[x y]]
  [[[x (dec y)] cmd-move-north]
   [[x (inc y)] cmd-move-south]
   [[(dec x) y] cmd-move-west]
   [[(inc x) y] cmd-move-east]])

(defn safe-rand-nth
  [coll]
  (if (empty? coll)
    nil
    (rand-nth coll)))

(defn next-open-neighbor
  [{:keys [zone pos]}]
  (some->> pos
           neighbors
           (filterv #(not (zone (first %))))
           safe-rand-nth))

(defn explore
  [{:keys [zone pos computer] :as droid}]
  (if-let [[target-pos move-cmd] (next-open-neighbor droid)]
    ;; Move success; update droid and process feedback
    (let [[computer' feedback] (computer-io computer move-cmd)]
      (-> droid
          (assoc :computer computer')
          (process-feedback move-cmd target-pos feedback)))

    ;; Attempt backtrack
    (let [curr-tile (zone pos)]
      (if (= curr-tile start)
        ;; Exploration is complete
        (assoc droid :halt true)

        ;; Backtrack command is written on the tile
        (let [[computer' _] (computer-io computer curr-tile)]
          (-> droid
              (assoc :computer computer')
              (update :pos move-by-cmd curr-tile)))))))

(core/defn-split bfs-fn
  [zone target | [[[x y :as pos] depth] & queue] seen]
  (if (= pos target)
    depth
    (recur (->> pos
                neighbors
                (map first)
                (filter #(and (not (seen %)) (not (= wall (zone %)))))
                (map #(vector % (inc depth)))
                (concat queue))
           (conj seen pos))))

(core/defn-split bfs-flood-fn
  [zone source | [[[x y :as pos] depth] & queue] seen]
  (let [remaining-neighbors (->> pos
                                 neighbors
                                 (map first)
                                 (filter #(and (not (seen %))
                                               (not (= wall (zone %))))))]
    (if (and (empty? queue) (empty? remaining-neighbors))
      depth
      (recur (->> remaining-neighbors
                  (map #(vector % (inc depth)))
                  (concat queue))
             (conj seen pos)))))

(let [{zone :zone oxygen-pos :oxygen} (-> input
                                          (program->computer '())
                                          init-droid
                                          (->> (iterate explore)
                                               (drop-while (complement :halt))
                                               first))
      bfs (bfs-fn zone oxygen-pos)
      bfs-flood (bfs-flood-fn zone oxygen-pos)]
  [(bfs [[[0 0] 0]] #{})
   (bfs-flood [[oxygen-pos 0]] #{})])
