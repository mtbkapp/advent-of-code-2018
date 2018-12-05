(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as sets]))

(def lower? #(Character/isLowerCase ^Character %))
(def to-lower #(Character/toLowerCase ^Character %))

#_(will-react? \c \c)
#_(will-react? \c \C)
#_(will-react? \C \c)
#_(will-react? \A \c)
#_(will-react? \A \C)
#_(will-react? \A \A)

(defn will-react?
  [x y]
  (and (some? x) (some? y)
       (= (to-lower x) (to-lower y))
       (not= (lower? x) (lower? y))))

#_(react "abBAccc")
(defn react
  [ps]
  (loop [[x y & next-ps] ps ls []]
    (if (some? x)
      (if (will-react? x y)
        ; go one char back in new sequence
        (if (empty? ls)
          (recur next-ps ls)
          (recur (cons (peek ls) next-ps) (pop ls)))
        ; put first char into result
        ; move forward only 1 char
        (recur (cons y next-ps) (conj ls x)))
      ls)))

(deftest test-react 
  (is (= [] (react "aA")))
  (is (= (seq "c") (react "bBc")))
  (is (= (seq "cccc") (react "cccc")))
  (is (= [] (react "abBA")))
  (is (= (seq "abAB") (react "abAB")))
  (is (= (seq "dabCBAcaDA") (react "dabAcCaCBAcCcaDA")))
  (is (empty? (react "abBcCdDDdeEA"))))

(defn lazy-read-chars*
  [rdr]
  (lazy-seq
    (let [c (.read rdr)]
      (cond (= c -1) (do (.close rdr) [])
            (= (char c) \newline) (lazy-read-chars rdr)
            :else (cons (char c) (lazy-read-chars rdr))))))

(defn lazy-read-chars
  [file]
  (lazy-read-chars* (io/reader file)))

#_(part1)
#_(time (part1))
(defn part1
  []
  (count (react (lazy-read-chars (io/resource "day5.txt")))))

(defn size-without-type 
  [polymer t]
  (->> polymer
       (remove #(= t (to-lower %)))
       (react)
       (count)))

#_(improve "dabAcCaCBAcCcaDA")
(defn improve
  [polymer]
  (->> (into #{} (map to-lower) polymer)
       (map (juxt identity (partial size-without-type polymer)))
       (reduce #(if (< (second %1) (second %2)) %1 %2))))


#_(time (part2))
(defn part2
  []
  (let [polymer (lazy-read-chars (io/resource "day5.txt"))
        [best-char new-size] (improve polymer)]
    new-size))
