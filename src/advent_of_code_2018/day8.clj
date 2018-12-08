(ns advent-of-code-2018.day8
  (:require [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.string :as string]))


(def N [2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2])


(defn read-input
  []
  (let [raw (slurp (io/resource "day8.txt"))]
    (into []
          (map #(Long/valueOf %))
          (string/split raw #"\s+"))))


(defn next-label
  [l]
  (char (inc (int l))))


(defn read-node
  [N start label]
  (let [cc (nth N start)
        mc (nth N (inc start))]
    (loop [cl (next-label label) i 0 pos (+ start 2) children []]
      (if (< i cc)
        (let [{:keys [length] :as child} (read-node N pos cl)]
          (recur (next-label cl)
                 (inc i)
                 (+ pos length)
                 (conj children child)))
        {:label label
         :length (+ (- pos start) mc)
         :children children 
         :meta (subvec N pos (+ pos mc))}))))


(defn collect-meta
  [{:keys [children meta]}]
  (concat meta (mapcat collect-meta children)))


#_(sum-meta N)
#_(sum-meta (read-input))
(defn sum-meta
  [input]
  (reduce + (collect-meta (read-node input 0 \A))))


#_(score (read-node N 0 \A))
#_(score (read-node (read-input) 0 \A))
(defn score
  [{:keys [children meta]}]
  (if (empty? children)
    (reduce + meta)
    (transduce
      (comp (map dec)
            (map (fn [i] (nth children i nil)))
            (map (fn [c] (if (nil? c) 0 (score c)))))
      +
      meta)))


