(ns advent-of-code-2018.day9
  (:import [advent_of_code_2018 Day9])
  (:gen-class))

(defn -main
  [& args]
  (doto (Day9.)
    (.printTransitions)
    (.test)
    (.part1)
    (.part2)))
