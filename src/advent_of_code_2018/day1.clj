(ns advent-of-code-2018.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))


(defn read-file
  []
  (->> (io/resource "day1_part1.txt")
       slurp
       string/split-lines
       (map #(Long/valueOf %))))

#_(part1 (read-file)) ; 430
(defn part1
  [drifts]
  (reduce (fn [freq drift]
            (+ freq drift))
          0
          drifts))

(defn freqs
  ([drifts] (freqs (cycle drifts) 0))
  ([[d & ds] freq]
   (lazy-seq (cons freq (freqs ds (+ freq d))))))

#_(part2 (read-file)) ;[462 139822]
(defn part2
  ([drifts] (part2 #{} (freqs drifts) 0))
  ([seen [f & fs] n]
   (cond (contains? seen f) [f n]
         (> n 1000000) nil
         :else (recur (conj seen f) fs (inc n)))))

