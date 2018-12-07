(ns advent-of-code-2018
  (:require [clojure.java.io :as io]
            [clojure.string :as string] 
            [clojure.set :as sets]))


(def input (slurp  (io/resource "day6.txt")))

(defn parse-line
  [line]
  (map (comp #(Long/valueOf %) string/trim)
       (string/split line #",")))

(def points (reduce (fn [points line]
                      (conj points (parse-line line)))
                    #{} 
                    (string/split-lines input)))


(def bounds (reduce (fn [[max-x max-y] [x y]]
            [(max max-x x) (max max-y y)])
          [0 0]
          points))

(def max-x (first bounds))
(def max-y (first bounds))

(defn mdist
  [[ax ay] [bx by]]
  (+ (Math/abs (- bx ax))
     (Math/abs (- by ay))))
(mdist [0 0] [1 5])

(defn closest
  [points p]
  ; choose a point in points that is closest to p
  (let [[[p0 d0] [p1 d1]] (->> points
                               (map (juxt identity (partial mdist p)))
                               (sort-by second))]
    (if (not= d0 d1)
      p0
      :tie)))

(time (closest points [max-x max-y]))  ; ~1ms
(/ (* max-x max-y) 1000.0)

(time
  (def grid
    (into {}
          (for [x (range (inc max-x))
                y (range (inc max-y))
                :let [p [x y]]
                :when (not (contains? points p))]
            [p (closest points p)]))))
(first grid)
(count grid)

; check that the choses closest points are input 
(every? points (remove #(= % :tie) (vals grid)))


(def unbounded-points
  (reduce (fn [unbounded [[x y :as p] closest]]
            (if (and (or (= x max-x) (zero? x)
                         (= y max-y) (zero? y))
                     (not= :tie closest))
              (conj unbounded closest)
              unbounded))
          #{}
          grid))

(not= unbounded-points points)
(sets/subset? unbounded-points points)
(sets/difference unbounded-points points)

(def bounded-grid (reduce (fn [ug [p closest]]
                            (if (or (contains? unbounded-points closest)
                                    (= :tie closest))
                              (dissoc ug p)
                              ug))
                          grid 
                          grid))

(count bounded-grid)


(def points-by-area (sort-by (comp - val) (frequencies (vals bounded-grid))))
(< (val (last points-by-area)) (val (first points-by-area)))
; solution to part 1, need to inc to add back previous excluded input point
(prn (inc (val (first points-by-area)))) ;3660


; part 2
(defn sum-dist
  [points p]
  (transduce (map (partial mdist p)) + 0 points))


(time
  (def grid
    (into {}
          (for [x (range (inc max-x))
                y (range (inc max-y))
                :let [p [x y]]]
            [p (sum-dist points p)]))))

(take 10 grid)

; part 2 solution
(prn (count (filter (comp #(< % 10000) val) grid)))
