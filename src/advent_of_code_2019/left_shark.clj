(ns advent-of-code-2019.left-shark
  (:require [clojure.string :as s]
            [advent-of-code-2019.core :as core]))

;; Core

(core/defn-split- consume-exactly
  [expected-string | {:keys [input]}]
  (when input
    (if (s/starts-with? input expected-string)
      [expected-string (subs input (count expected-string) (count input))]
      [nil input])))

(defn- consume-regex
  [expected-regex]
  (let [anchored-pattern (re-pattern (str "^" (str expected-regex)))]
    (fn [{:keys [input]}]
      (when input
        (let [matcher (re-matcher anchored-pattern input)
              match (re-find matcher)
              match' (if (coll? match) (first match) match)]
          (if match'
            [match' (subs input (count match') (count input))]
            [nil input]))))))

(def ^:private pattern? (comp (partial = java.util.regex.Pattern) type))

(defn- build-consumer [consumer]
  (cond
    (string? consumer) (consume-exactly consumer)
    (pattern? consumer) (consume-regex consumer)
    :else (throw (RuntimeException. "Consumer must be a string or regex."))))

(defn- parsing [input]
  {:input input})

(defn- parse-chunk [consumer chunk-handler failure-handler]
  (let [consumer' (build-consumer consumer)]
    (fn [parse-map]
      (let [[parsed remainder] (consumer' parse-map)]
        (if parsed
          (chunk-handler parse-map parsed remainder)
          (failure-handler parse-map))))))

(defn- discard-chunk [parse-map parsed remainder]
  (assoc parse-map :input remainder))

(core/defn-split- save-chunk
  [component-name transform | parse-map parsed remainder]
  (-> parse-map
      (assoc :input remainder)
      (assoc-in [:components component-name] (transform parsed))))

(core/defn-split- update-chunk
  [component-name update-f parse-map parsed remainder]
  (-> parse-map
      (assoc :input remainder)
      (update-in [:components component-name] #(update-f % parsed))))

(def ^:private keep-old identity)

(defn ^:private kill-parse [parse-map] nil)

(defn attempt
  ([consumer]
   (parse-chunk consumer discard-chunk keep-old))
  ([consumer component-name]
   (parse-chunk consumer (save-chunk component-name identity) keep-old))
  ([consumer component-name transform]
   (parse-chunk consumer (save-chunk component-name transform) keep-old)))

(def ? attempt)

(defn demand
  ([consumer]
   (parse-chunk consumer discard-chunk kill-parse))
  ([consumer component-name]
   (parse-chunk consumer (save-chunk component-name identity) kill-parse))
  ([consumer component-name transform]
   (parse-chunk consumer (save-chunk component-name transform) kill-parse)))

(def ! demand)

;; A combinator that takes a series of other chunks, and either parses them all in sequence, or
;;  skips them all if they cannot be parsed.
(core/defn-split branch
  [& chunks | parse-map]
  (let [parse-map' (reduce #(%2 %1) parse-map chunks)]
    (or parse-map' parse-map)))

(def < branch)

(core/defn-split parse
  [& chunks | input]
  (reduce #(%2 %1) (parsing input) chunks))

;; Useful premades

(def demand-consumed (demand #"$"))

(def $ demand-consumed)

(defn int!
  [component-name]
  (demand #"-?\d+" component-name core/parse-int))

(defn int?
  [component-name]
  (attempt #"-?\d+" component-name core/parse-int))

;; To implement sequence consumer, you'll need to adapt your interface.
;; Calls would instead look something like the following.
;;
;; Default: ls/discard
;; (ls/! #"\d+")
;; (ls/! #"\d+" ls/discard)
;;
;; Save (by overwriting component key): ls/assoc-to or ls/save; bare keyword is translated to this
;; (ls/! #"\d+" :xyz)
;; (ls/! #"\d+" (ls/assoc-to :xyz))
;; (ls/! #"\d+" (ls/assoc-to :xyz transform))
;;
;; Update (reduce fn w/ current component value): ls/update-to
;; (ls/! #"\d+" (ls/update-to :xyz #(vec (conj %1 %2))))
;;
;; Then seq* or seq+ can be added as premades:
;; (defn seq* [& chunks]
;;   (fn [parse-map]
;;     (if-let [parse-map' (reduce #(%2 %1) parse-map chunks)]
;;       (recur parse-map')
;;       parse-map)))
;;
;; (defn seq+ [& chunks]
;;   (let [rest-seq* (seq* chunks)]
;;     (fn [parse-map]
;;       (when-let [parse-map' (reduce #(%2 %1) parse-map chunks)]
;;         (rest-seq* parse-map'))))
;;
;; Side effects of this change: you can handle some parsed items separated by distance.
;; (ls/< (ls/! "First:") (ls/! #"\w+" :words vector))
;; ...
;; (ls/! "\w+" (ls/update-to :words (comp vec conj)))
