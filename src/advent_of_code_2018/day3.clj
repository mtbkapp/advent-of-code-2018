(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]))


#_(prn (re-seq line-pattern "#13 @ 928,664: 18x15"))
(def line-pattern #"^\#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$")

#_(take 2 (read-claims))
(defn read-claims
  []
  (with-open [rdr (io/reader (io/resource "day3.txt"))]
    (doall (map (comp (fn [{:keys [left top width height] :as claim}]
                        (assoc claim 
                               :right (+ left width)
                               :bottom (+ top height)))
                      (fn [line]
                        (->> (re-seq line-pattern line)
                             first
                             rest
                             (map #(Long/valueOf %))
                             (zipmap [:id :left :top :width :height]))))
                (line-seq rdr)))))

(defn coords-in-claim
  [{:keys [id left top width height]}]
  (for [x (range left (+ left width))
        y (range top (+ top height))]
    [id [x y]]))

(defn rasterize 
  [claims]
  (transduce (mapcat coords-in-claim)
             (completing 
               (fn [coords [id xy]]
                 (update coords xy (fnil conj #{}) id)))
             {}
             claims))

#_(overlapping-area (read-claims)) ; 107820
(defn overlapping-area
  [claims]
  (count (filter #(<= 2 (count (val %)))
                 (rasterize claims))))

(defn range-overlap?
  [[start-a end-a :as a] [start-b end-b :as b]]
  (if (< start-b start-a)
    (recur b a)
    (< start-b end-a)))

(def x-range (juxt :left :right))
(def y-range (juxt :top :bottom))

(defn overlap?
  [x y]
  (if (not= (:id x) (:id y))
    (and (range-overlap? (x-range x) (x-range y))
         (range-overlap? (y-range x) (y-range y)))))

(defn overlaps-any?
  [c all]
  (some (partial overlap? c) all))

#_(first-nonoverlapping (read-claims)) ; id = 661
(defn first-nonoverlapping
  [claims]
  (some (fn [c]
          (if (not (overlaps-any? c claims)) c))
        claims))

