(ns advent-of-code-2018.day11
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]))


(def serial-no 7315)

(defn hundreds-digit
  [n]
  (let [str-n (str n)]
    (-> str-n
        (.charAt (- (count str-n) 3))
        str
        Long/valueOf)))

(defn power-level 
  [[x y :as p] sn]
  (let [rack-id (+ 10 x)]
    (-> (* rack-id y)
        (+ sn)
        (* rack-id)
        (hundreds-digit)
        (- 5))))

(def grid-points
  (for [y (range 0 301)
        x (range 0 301)]
    [x y]))

(def power-grid
  (into {} 
        (map (juxt identity 
                   #(power-level % serial-no)))  
                grid-points))

(defn corner->section
  [[x y]]
  (when (and (<= (+ x 2) 300)
             (<= (+ y 2) 300))
    (for [dx [0 1 2]
          dy [0 1 2]]
      [(+ x dx) (+ y dy)])))

(defn section-power
  [grid section]
  (reduce (fn [sum p]
            (+ sum (get grid p)))
          0
          section))

(def section->corner ffirst)

; 946ms
(time
  (def part-1 (->> (into [] 
                         (comp (map corner->section)
                               (remove nil?)
                               (map (juxt identity
                                          (partial section-power power-grid))))
                         grid-points)
                   (sort-by (comp - second))
                   first
                   section->corner)))

(max-square-size [300 300])
(max-square-size [200 300])
(max-square-size [200 200])
(max-square-size [0 0])
(defn max-square-size
  [[x y]]
  (min (- 301 x) (- 301 y)))

; https://en.wikipedia.org/wiki/Summed-area_table
(def summed-area-table
  (reduce (fn [table [x y :as p]]
            (assoc table p (+ (get power-grid p)
                              (get table [x (dec y)] 0)
                              (get table [(dec x) y] 0)
                              (- (get table [(dec x) (dec y)] 0)))))
          {}
          grid-points))

(defn square-power
  [area-table [x y :as p] size]
  (let [size (dec size)
        A [(dec x) (dec y)]
        B [(+ x size) (dec y)]
        C [(dec x) (+ y size)]
        D [(+ x size) (+ y size)]]
    (+ (get area-table D)
       (- (get area-table B 0))
       (- (get area-table C 0))
       (get area-table A 0))))

(= (summed-area-table [300 300])
   (square-power summed-area-table [0 0] 301))

(= (summed-area-table [0 0])
   (square-power summed-area-table [0 0] 1))

; 473ms
(time
  (def part1_2 (->> grid-points
                    (filter #(<= 3 (max-square-size %)))
                    (map (juxt identity #(square-power summed-area-table % 3)))
                    (sort-by (comp - second))
                    ffirst)))

(= part1_2 part-1)

; 78665ms
(time
  (def part2 (->> grid-points
                  (mapcat (fn [p]
                            (map (partial vector p)
                                 (range 1 (inc (max-square-size p))))))
                  (map (juxt identity 
                             (fn [[p size]]
                               (square-power summed-area-table p size))))
                  (sort-by (comp - second))
                  (first))))
(prn part2)
