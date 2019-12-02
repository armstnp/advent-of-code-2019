(ns advent-of-code-2019.day2
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (-> "day2.txt"
               core/read-input
               str/trim
               (str/split #",")
               (->> (mapv core/parse-int))))

(defn run-instr
  [{:keys [memory ip] :as computer}]
  (let [[op in-addr-1 in-addr-2 out-addr] (take 4 (drop ip memory))
        memory' (case op
                  1 (assoc memory out-addr (+ (memory in-addr-1) (memory in-addr-2)))
                  2 (assoc memory out-addr (* (memory in-addr-1) (memory in-addr-2)))
                  99 memory
                  (str "Unknown op-code " op))
        ip' (+ ip 4)]
    (if (= op 99)
      (assoc computer :halt true)
      (assoc computer :memory memory' :ip ip'))))

(defn run-program
  [program]
  ((->> {:memory program :ip 0}
       (iterate run-instr)
       (drop-while #(not (:halt %)))
       first
       :memory)
   0))

(defn run-noun-verb
  [program noun verb]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)
      run-program))

(run-noun-verb input 12 2)

(def noun-verb-combo (fn [[noun verb]] (+ (* 100 noun) verb)))

(-> (for [noun (range 100)
          verb (range 100)
          :when (= (run-noun-verb input noun verb) 19690720)]
      [noun verb])
    first
    noun-verb-combo)
