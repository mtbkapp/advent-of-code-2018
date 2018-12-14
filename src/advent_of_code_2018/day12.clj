(ns advent-of-code-2018.day12
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.test :refer :all]))

(defn parse-patterns
  [patterns]
  (reduce (fn [ret pattern]
            (let [[[_ pattern result]] (re-seq #"([#\.]{5})\s=>\s([#\.])" pattern)]
              (assoc ret (seq pattern) (first result))))
          {}
          patterns))

(defn parse
  [input]
  (let [[init-line _ & patterns] (string/split-lines input)]
    (let [right (drop 15 init-line)]
      {:initial-state (concat (repeat 200 \.) 
                              right
                              (repeat 200 \.))
       :right right
       :patterns (parse-patterns patterns)})))

(def parsed-input (parse (slurp (io/resource "day12.txt"))))


(defn pad
  [section]
  (concat section
          (repeat (- 5 (count section)) \.)))

(defn sum-alive
  [state i]
  (reduce (fn [[sum i] pot]
            (if (= \# pot)
              [(+ sum i) (inc i)]
              [sum (inc i)]))
          [0 i]
          state))

(def test-patterns-s "...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #")

(def test-patterns (parse-patterns (string/split-lines test-patterns-s)))

(defn next-state
  [patterns state]
  (map (comp #(get patterns % \.) pad) 
       (partition-all 5 1 (list* \. \. state))))

; 325
#_(part-1 {:initial-state (seq "...#..#.#..##......###...###...........")
           :patterns test-patterns 
           :generations 20
           :first-pot-num -3})
; 1696
#_(part-1 (assoc parsed-input
                 :generations 20
                 :first-pot-num -200))
(defn part-1
  [{:keys [initial-state patterns first-pot-num generations] :as input}]
  (let [last-state (->> initial-state
                        (iterate (partial next-state patterns))
                        (take (inc generations))
                        (last))
        sum (first (sum-alive last-state first-pot-num))]
    sum))



(with-open [wr (io/writer "nums.csv")]
  (binding [*out* wr]
    (doseq [x (map (comp first #(sum-alive % -200))
                   (take 500 (iterate (partial next-state patterns)
                                      (:initial-state parsed-input))))]
      (println x))))

; Plotting the numbers shows that eventually the slope of the line is stable at 36 plants per generation


(def gen-200-sum
  (part-1 (assoc parsed-input
                 :generations 200
                 :first-pot-num -200)))
(prn gen-200-sum)

(def part-2 (+ (* 36 (- 50000000000 200)) gen-200-sum))
(prn part-2) ; 1799999999458
