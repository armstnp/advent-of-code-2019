(ns advent-of-code-2019.day14
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (->> "day14.txt" core/read-input str/split-lines))

(defn parse-chem
  [quant-chem]
  (let [[quant chem] (str/split quant-chem #" ")]
    {:quantity (core/parse-int quant)
     :chemical chem}))

(defn parse-inputs
  [rxn-inputs]
  (map parse-chem (str/split rxn-inputs #", ")))

(defn parse-reaction
  [line]
  (let [[rxn-inputs rxn-output] (str/split line #" => ")
        rxn-inputs (parse-inputs rxn-inputs)
        {out-quant :quantity out-chem :chemical} (parse-chem rxn-output)]
    [out-chem {:out-quant out-quant
               :inputs rxn-inputs}]))

(defn parse-reactions
  [input]
  (into {} (map parse-reaction input)))

(defn expand-chemical
  [reactions {[{requested-chem :chemical requested-quant :quantity :as output}] :queue
              :keys [spares]
              :as state}]
  (if (= requested-chem "ORE")
    (throw (IllegalArgumentException. "Cannot expand 'ORE'"))
    (let [{:keys [out-quant inputs]} (reactions requested-chem)
          spare-quant (get spares requested-chem 0)
          reaction-multiplier (-> requested-quant
                                  (- spare-quant)
                                  (max 0)
                                  (/ out-quant)
                                  Math/ceil
                                  int)
          expanded-inputs (->> inputs
                               (map #(update % :quantity * reaction-multiplier))
                               (filter (comp (complement zero?) :quantity)))
          available-quant (+ spare-quant (* reaction-multiplier out-quant))]
      (-> state
          (update :queue (comp #(concat % expanded-inputs) next))
          (assoc-in [:spares requested-chem] (max 0 (- available-quant requested-quant)))))))

(core/defn-split reduce-to-ore-fn
  [reactions | {[{:keys [chemical quantity] :as next-chem}] :queue
                :keys [ore]
                :as state}]
  (cond
    (nil? next-chem) state
    (= chemical "ORE") (-> state
                           (update :ore + quantity)
                           (update :queue next))
    :else (expand-chemical reactions state)))

(defn solve
  [reduce-to-ore fuel]
  (->> {:queue [{:chemical "FUEL" :quantity fuel}]
        :ore 0
        :spares {}}
       (iterate reduce-to-ore)
       (drop-while :queue)
       first))

(def single-fuel-max-cost
  (-> input
      parse-reactions
      reduce-to-ore-fn
      (solve 1)
      :ore))

single-fuel-max-cost

(defn solve-for-max-ore
  [reduce-to-ore max-ore state used-fuel]
  (let [state' (-> state
                   (assoc :queue [{:chemical "FUEL" :quantity 1}])
                   (->>
                    (iterate reduce-to-ore)
                    (drop-while :queue)
                    first))]
    (if (> (:ore state') max-ore)
      used-fuel
      (recur reduce-to-ore max-ore state' (inc used-fuel)))))

(def trillion 1000000000000N)
(let [reduce-to-ore (-> input parse-reactions reduce-to-ore-fn)
      min-fuel (Math/ceil (* 2.48 (quot trillion single-fuel-max-cost))) ;; Narrowed by hand - automate later
      min-fuel-state (solve reduce-to-ore min-fuel)]
  (solve-for-max-ore
   reduce-to-ore
   trillion
   min-fuel-state
   min-fuel))
