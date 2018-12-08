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

(prn (apply str part1)) ;LAPFCRGHVZOTKWENBXIMSUDJQY 

(job-len \A)
(job-len \Z)
(defn job-len
  [job]
  (+ 61 (- (int job) (int \A))))

(defn job-len-test
  [job]
  (inc (- (int job) (int \A))))

(defn free-workers
  [workers]
  (map key (filter (comp nil? val) workers)))

(tick-workers {1 nil 2 [\A 3] 3 [\B 1]})
(defn tick-workers 
  [workers]
  (reduce (fn [[workers finished :as acc] [id [job-id len :as job]]]
            (if (some? job)
              (if (= 1 len)
                ; worker finished job
                [(assoc workers id nil) (conj finished job-id)]
                ; worker made progress on job
                [(assoc workers id [job-id (dec len)]) finished])
              ; worker idle
              acc))
          [workers #{}]
          workers))

(def test-graph
  (-> (loom.graph/digraph)
      (loom.graph/add-edges [\C \A]
                            [\C \F]
                            [\A \B]
                            [\A \D]
                            [\B \E]
                            [\D \E]
                            [\F \E])))
(loom.io/view test-graph)


; G = original graph
; wG = working graph
; next-jobs = nodes with in-degree = 0 and all predecessors are in the done set 
(next-jobs test-graph (loom.graph/remove-nodes test-graph \C) #{\C})
(defn next-jobs
  [G wG done]
  (->> (loom.graph/nodes wG)
       (filter #(zero? (loom.graph/in-degree wG %)))
       (filter #(every? done (loom.graph/predecessors G %)))
       (sort)))

(defn schedule-work
  [workers free jobs job-len]
  (->> (map vector free jobs)
       (reduce (fn [workers [w j]]
                 (assoc workers w [j (job-len j)]))
               workers)))


(do-work test-graph {1 nil 2 nil} job-len-test true)
(do-work graph {1 nil 2 nil 3 nil 4 nil 5 nil} job-len false) ; 936
(defn do-work
  [G workers job-len log]
  (let [all-n (into #{} (loom.graph/nodes G))]
    (loop [wG G done #{} workers workers t -1]
      (when log
        (println t " " workers))
      (if (= done all-n)
        t
        (let [[next-workers finished-jobs] (tick-workers workers)
              next-done (sets/union done finished-jobs)
              free-workers (free-workers next-workers)
              jobs (take (count free-workers)
                         (next-jobs G wG next-done))
              nn-workers (schedule-work next-workers free-workers jobs job-len)]
          (recur (apply loom.graph/remove-nodes wG jobs)
                 next-done
                 nn-workers
                 (inc t)))))))
