(ns advent-of-code-2018.day3
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as sets])
  (:import [java.time LocalDateTime Duration]))


(def line-pattern #"^\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})\] (wakes|falls|Guard \#(\d+))")

#_(re-seq line-pattern "[1518-03-19 00:41] wakes up")
#_(re-seq line-pattern "[1518-04-15 00:10] falls asleep")
#_(re-seq line-pattern "[1518-02-06 23:52] Guard #3109 begins shift")

; (["[1518-02-06 23:52] Guard #3109" "1518" "02" "06" "23" "52" "Guard #3109" "3109"])  

#_(pprint (take 10 (read-events)))
(defn read-events
  []
  (with-open [rdr (io/reader (io/resource "day4.txt"))]
    (doall (->> (line-seq rdr) 
                (map (fn [line]
                       (let [[_ year month day hour minute ev-type guard-id]
                             (first (re-seq line-pattern line))
                             event {:inst (LocalDateTime/of (Integer/valueOf year)
                                                            (Integer/valueOf month)
                                                            (Integer/valueOf day)
                                                            (Integer/valueOf hour)
                                                            (Integer/valueOf minute))
                                    :ev-type (cond (= ev-type "wakes") :ev/wake
                                                   (= ev-type "falls") :ev/falls
                                                   (some? guard-id) :ev/shift
                                                   :else :ev/unknown)}]
                         (if (some? guard-id)
                           (assoc event :guard-id guard-id)
                           event))))
                (sort-by :inst)))))

(defn find-intervals
  [events]
  (:intervals
    (reduce (fn [{:keys [sleep-start current-guard-id last-ev-type] :as acc}
                 {:keys [ev-type guard-id inst] :as event}]
              (cond (= ev-type :ev/shift) (assoc acc :current-guard-id guard-id)
                    (= ev-type :ev/falls) (assoc acc :sleep-start inst)
                    (= ev-type :ev/wake) 
                    (update-in acc [:intervals current-guard-id] 
                               (fnil conj [])
                               [sleep-start inst]) 
                    :else acc))
            {:current-guard-id nil
             :sleep-start nil
             :intervals {}}
            events)))

(defn intervals->duration
  [inters]
  (reduce (fn [total [start end]]
            (+ total (.toMinutes (Duration/between start end))))
          0
          inters))

(defn sleepiest-guard 
  [intervals]
  (:guard-id
    (reduce (fn [mx [guard-id inters]]
              (let [d (intervals->duration inters)]
                (if (< (:sleep mx) d)
                  {:guard-id guard-id :sleep d}
                  mx)))
            {:guard-id nil :sleep 0}
            intervals)))

(defn sleepiest-minute
  [intervals guard-id]
  (->> (get intervals guard-id)
       (mapcat (fn [[start end]]
                 (range (.getMinute start) (.getMinute end))))
       (frequencies)
       (sort-by val)
       (last)))

#_(strategy1 (read-events))
(defn strategy1
  [events]
  (let [intervals (find-intervals events)
        guard (sleepiest-guard intervals)
        [minute freq] (sleepiest-minute intervals guard)]
    (* (Long/valueOf guard) minute)))


#_(strategy2 (read-events))
(defn strategy2
  [events]
  (let [intervals (find-intervals events)
        [guard-id [minute freq]] (->> (keys intervals)
                                      (map (juxt identity #(sleepiest-minute intervals %)))
                                      (sort-by (comp val second))
                                      (last))]
    (* (Long/valueOf guard-id) minute)))


