(ns advent-of-code-2018.day10
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))


(def ex-input "position=< 9,  1> velocity=< 0,  2>
position=< 7,  0> velocity=<-1,  0>
position=< 3, -2> velocity=<-1,  1>
position=< 6, 10> velocity=<-2, -1>
position=< 2, -4> velocity=< 2,  2>
position=<-6, 10> velocity=< 2, -2>
position=< 1,  8> velocity=< 1, -1>
position=< 1,  7> velocity=< 1,  0>
position=<-3, 11> velocity=< 1, -2>
position=< 7,  6> velocity=<-1, -1>
position=<-2,  3> velocity=< 1,  0>
position=<-4,  3> velocity=< 2,  0>
position=<10, -3> velocity=<-1,  1>
position=< 5, 11> velocity=< 1, -2>
position=< 4,  7> velocity=< 0, -1>
position=< 8, -2> velocity=< 0,  1>
position=<15,  0> velocity=<-2,  0>
position=< 1,  6> velocity=< 1,  0>
position=< 8,  9> velocity=< 0, -1>
position=< 3,  3> velocity=<-1,  1>
position=< 0,  5> velocity=< 0, -1>
position=<-2,  2> velocity=< 2,  0>
position=< 5, -2> velocity=< 1,  2>
position=< 1,  4> velocity=< 2,  1>
position=<-2,  7> velocity=< 2, -2>
position=< 3,  6> velocity=<-1, -1>
position=< 5,  0> velocity=< 1,  0>
position=<-6,  0> velocity=< 2,  0>
position=< 5,  9> velocity=< 1, -2>
position=<14,  7> velocity=<-2,  0>
position=<-3,  6> velocity=< 2, -1>")


#_(parse ex-input)
(defn parse
  [raw]
  (map (comp (fn [[px py vx vy]]
               {:pos [px py]
                :v [vx vy]})
             (fn [line]
               (into []
                     (comp (map string/trim)
                           (filter (complement empty?))
                           (map #(Long/valueOf %)))
                     (re-seq #"[\d\-\s]+" line))))
       (string/split-lines raw)))


(defn add
  [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])


#_(next-state (parse ex-input))
(defn next-state
  [state]
  (map (fn [{:keys [pos v] :as point}]
         (assoc point :pos (add pos v)))
       state))


#_(find-bounds (parse ex-input))
(defn find-bounds
  [points]
  (reduce (fn [[[min-x max-x] [min-y max-y]] {[x y] :pos}]
            [[(min min-x x) (max max-x x)]
             [(min min-y y) (max max-y y)]])
          [[Long/MAX_VALUE Long/MIN_VALUE]
           [Long/MAX_VALUE Long/MIN_VALUE]]
          points))


(defn size
  [points]
  (if (some? points)
    (let [[[min-x max-x] [min-y max-y]] (find-bounds points)]
      (* (Math/abs (- max-x min-x))
         (Math/abs (- min-y max-y))))
    0))


#_(doseq [s (take 5 (iterate next-state (parse ex-input)))]
    (vis-points s)
    (println))


(defn vis-points
  [state]
  (let [[[min-x max-x] [min-y max-y]] (find-bounds state)
        points (into #{} (map :pos) state)]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-y (inc max-x))]
        (print (if (contains? points [x y]) "#" ".")))
      (println))))


(defn find-size-minima
  [last-state n]
  (let [next-state (next-state last-state)]
    (if (> (size next-state) (size last-state))
      [last-state n]
      (recur next-state (inc n)))))


#_(solve)
(defn solve
  []
  (let [N (parse (slurp (io/resource "day10.txt")))
        [ps n] (find-size-minima N 0)]
    (println "Found message at n = " n)
    (vis-points ps)))

