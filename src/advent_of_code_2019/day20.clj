(ns advent-of-code-2019.day20
  (:require [advent-of-code-2019.core :as core]
            [clojure.string :as str]))

(def input (->> "day20.txt" core/read-input str/split-lines))

(defn input->maze
  [input]
  (->> input
       (drop 2)
       (drop-last 2)
       (mapv (comp vec (partial drop 2) (partial drop-last 2)))))

(defn parse-vertical-portals
  [y rows]
  (->> rows
       (apply map str)
       (map-indexed #(vector %2 [y %1]))
       (filter (comp (partial re-matches #"[A-Z]+") first))))

(defn parse-horizontal-portals
  [x cols]
  (->> cols
       (map-indexed #(vector (apply str %2) [%1 x]))
       (filter (comp (partial re-matches #"[A-Z]+") first))))

(defn parse-maze
  [input ring-width]
  (let [maze (input->maze input)
        outer-top-portals (parse-vertical-portals 0 (->> input (take 2) (map (partial drop 2))))
        outer-left-portals (parse-horizontal-portals 0 (->> input (drop 2) (map (partial take 2))))
        outer-bottom-portals (parse-vertical-portals (dec (count maze))
                                                     (->> input (take-last 2) (map (partial drop 2))))
        outer-right-portals (parse-horizontal-portals (dec (count (first maze)))
                                                      (->> input (drop 2) (map (partial take-last 2))))
        inner-top-portals (parse-vertical-portals (dec ring-width) (->> maze
                                                                        (drop ring-width)
                                                                        (take 2)))
        inner-left-portals (parse-horizontal-portals (dec ring-width) (map (comp (partial take 2)
                                                                              (partial drop ring-width))
                                                                           maze))
        inner-bottom-portals (parse-vertical-portals (- (count maze) ring-width)
                                                     (->> maze
                                                          (take-last (+ ring-width 2))
                                                          (take 2)))
        inner-right-portals (parse-horizontal-portals (- (count (first maze)) ring-width)
                                                      (map (comp (partial take 2)
                                                              (partial take-last (+ ring-width 2)))
                                                           maze))
        all-portals-raw (concat outer-top-portals outer-left-portals
                                outer-bottom-portals outer-right-portals
                                inner-top-portals inner-left-portals
                                inner-bottom-portals inner-right-portals)
        portal-map (-> (group-by first all-portals-raw)
                       (dissoc "AA")
                       (dissoc "ZZ")
                       vals
                       (->>
                        (mapv (partial mapv second))
                        (mapcat #(vector % [(second %) (first %)]))
                        vec
                        (into {})))
        entrance (->> all-portals-raw (filter (comp (partial = "AA") first)) first second)
        exit (->> all-portals-raw (filter (comp (partial = "ZZ") first)) first second)]
    {:maze maze
     :entrance entrance
     :exit exit
     :portals portal-map}))

(core/defn-split bfs-fn
  [{:keys [maze portals exit]} | {[{:keys [pos distance]} & rest-queue] :queue
                                  :keys [seen] :as state}]
  (cond
    (= pos exit) distance
    (seen pos) (update state :queue next)
    :else (let [immediate-neighbors (->> [[-1  0]
                                          [ 1  0]
                                          [ 0 -1]
                                          [ 0  1]]
                                         (mapv (partial mapv + pos))
                                         (filter #(and (not (seen %)) (= \. (get-in maze %)))))
                portal-neighbor (when-some [neighbor (portals pos)]
                                  (when-not (seen neighbor) neighbor))
                neighbors (if portal-neighbor
                            (conj immediate-neighbors portal-neighbor)
                            immediate-neighbors)
                queue' (->> neighbors
                            (map #(hash-map :pos % :distance (inc distance)))
                            (concat rest-queue))]
            {:queue queue'
             :seen (conj seen pos)})))

(let [{:keys [entrance exit] :as maze} (parse-maze input 35)
      bfs (bfs-fn maze)
      init-state {:queue [{:pos entrance :distance 0}]
                  :seen #{}}]
  (->> init-state
       (iterate bfs)
       (drop-while (complement number?))
       first))

(defn edge?
  [[y x] maze]
  (or (zero? y)
      (zero? x)
      (= y (dec (count maze)))
      (= x (dec (count (first maze))))))

(core/defn-split recursive-bfs-fn
  [{:keys [maze portals exit]} | {[{:keys [pos distance]} & rest-queue] :queue
                                  :keys [seen] :as state}]
  (cond
    (= pos exit) distance
    (seen pos) (update state :queue next)
    :else (let [immediate-neighbors (->> [[-1  0 0]
                                          [ 1  0 0]
                                          [ 0 -1 0]
                                          [ 0  1 0]]
                                         (mapv (partial mapv + pos))
                                         (filter #(and (not (seen %)) (= \. (get-in maze (take 2 %))))))
                portal-neighbor (when-some [neighbor (portals (vec (take 2 pos)))]
                                  (let [curr-depth (nth pos 2)
                                        new-depth (if (edge? neighbor maze)
                                                    (inc curr-depth) ;; outer shell of new layer
                                                    (dec curr-depth)) ;; inner shell of prior layer
                                        neighbor' (conj neighbor new-depth)]
                                    (when-not (or (< new-depth 0) (seen neighbor'))
                                      neighbor')))
                neighbors (if portal-neighbor
                            (conj immediate-neighbors portal-neighbor)
                            immediate-neighbors)
                queue' (->> neighbors
                            (map #(hash-map :pos % :distance (inc distance)))
                            (concat rest-queue))]
            {:queue queue'
             :seen (conj seen pos)})))

(let [{:keys [entrance exit] :as maze} (-> (parse-maze input 35)
                                           (update :exit conj 0)
                                           (update :entrance conj 0))
      bfs (recursive-bfs-fn maze)
      init-state {:queue [{:pos entrance :distance 0}]
                  :seen #{}}]
  (->> init-state
       (iterate bfs)
       (drop-while (complement number?))
       first))
