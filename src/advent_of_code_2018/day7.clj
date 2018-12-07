(ns advent-of-code-2018.day7
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as sets]
            [loom.graph]
            [loom.alg]
            [loom.io]))



(def graph (->> (slurp (io/resource "day7.txt"))
                string/split-lines
                (map (fn [s]
                       (filter #(Character/isUpperCase %) (rest s))))
                (reduce (fn [g [x y]]
                          (-> g 
                              (loom.graph/add-nodes x y)
                              (loom.graph/add-edges [x y])))
                        (loom.graph/digraph))))


(def part1 (loop [g graph out []]
            (let [n (first (sort (filter #(zero? (loom.graph/in-degree g %)) 
                                         (loom.graph/nodes g))))]
              (if (nil? n)
                out
                (recur (loom.graph/remove-nodes g n)
                       (conj out n))))))

(prn (apply str part1))
