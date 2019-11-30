(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io]))

(defn read-input
  [filename]
  (slurp (io/resource filename)))

(def zero-char-val (int \0))

(defn parse-int
  [n]
  (cond
    (string? n) (Integer/parseInt n)
    (char? n) (- (int n) zero-char-val)))

(defn transpose
  "Transposes the given nested sequence into nested vectors, as
  in matrix transposition.  E.g., (transpose [[1 2 3] [4 5 6]])
  would return [[1 4] [2 5] [3 6]]."
  [s]
  (vec (apply map vector s)))

(defn find-index
  "Returns the index of the first occurrence of n in coll."
  [coll n]
  (->> coll
    (map vector (range))
    (drop-while #(not= n (second %)))
    ffirst))

(defn reverse-lookup
  "Returns a key corresponding to the value v in map m.  There is no guarantee
  as to which key will be selected."
  [m v]
  (ffirst (filter #(= v (second %)) m)))

(defn rotate-left
  [n coll]
  (->> coll
    cycle
    (drop n)
    (take (count coll))))

(defn rotate-right
  [n coll]
  (rotate-left (- (count coll) (mod n (count coll))) coll))

(defn clamp
  [n min-val max-val]
  (max min-val (min max-val n)))

(defn update-by
  [m-v m-f]
  (reduce (fn [m [k f]] (update m k f)) m-v m-f))

(defn map-vals
  [f m]
  (reduce #(update %1 %2 f) m (keys m)))

(defn iterate-to
  [incomplete? & iter-args]
  (->> (apply iterate iter-args)
    (drop-while incomplete?)
    first))

(defn split-over
  "Eagerly splits the given finite collection at points where
  the given predicate is true, excluding the splitting elements
  themselves."
  [pred coll]
  (let [[a b-with-splitter] (split-with pred coll)
         b (next b-with-splitter)]
    (if b
      (cons a (split-over pred b))
      (list a))))

(defn non-pipe-sym?
  [x]
  (not= '| x))

(defmacro -split-fn-variadic
  [args forms]
  (let [[first-args & rest-args] (reverse (split-over non-pipe-sym? args))
        initial-form `(fn [~@first-args] ~@forms)]
    (loop [inner-f initial-form, args rest-args]
      (if args
        (recur `(fn [~@(first args)] ~inner-f) (next args))
        inner-f))))

(defmacro split-fn
  "Splits a function's arguments by the pipe symbol, creating a series of
  coarsely curried functions that accept each set of resulting arguments
  in turn, evaluating the provided forms once all arguments have been
  satisfied.
  E.g. (split-fn [a b | c d] (* a b c d))
    => (fn [a b] (fn [c d] (* a b c d))"
  [args & forms]
  `(-split-fn-variadic ~args ~forms))

(defmacro defn-split
  "Defines a split function with the given name.
  See: split-fn"
  [name args & body]
  `(def ~name
     (split-fn ~args ~@body)))

(defmacro defn-split-
  "Defines a private split function with the given name.
  See: split-fn"
  [name args & body]
  (list* `(def ~(with-meta name (assoc (meta name) :private true))
            (split-fn ~args ~@body))))
