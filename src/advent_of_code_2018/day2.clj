(ns advent-of-code-2018.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as sets]))

(defn read-box-ids
  []
  (->> (io/resource "day2.txt")
       slurp
       string/split-lines))

#_(part1 (read-box-ids)) ; 8715
(defn part1
  [box-ids]
  (transduce (comp (map frequencies)
                   (map sets/map-invert))
             (fn 
               ([counts freqs]
                (cond-> counts
                  (contains? freqs 3) (update 3 inc)
                  (contains? freqs 2) (update 2 inc)))
               ([{three 3 two 2}] 
                (* three two)))
             {3 0, 2 0}
             box-ids))

#_(twins? "axcye" "abcde")
#_(twins? "fghij" "fguij")
(defn twins?
  [b1 b2]
  (= 1 (reduce + (map #(if (= %1 %2) 0 1) b1 b2))))

#_(remove-diff "fghij" "fguij")
(defn remove-diff
  [b1 b2]
  (apply str (->> (map vector b1 b2)
                  (remove #(apply not= %))
                  (map first))))

#_(prn (part2 (read-box-ids)))
(defn part2
  [box-ids]
  (-> (for [b1 box-ids b2 box-ids :when (twins? b1 b2)]
        (remove-diff b1 b2))
      first))

