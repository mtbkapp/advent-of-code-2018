(ns advent-of-code-2018.day13
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [loom.graph :as graph]))


#_(println (first test-states))
(def test-states
  (->> (slurp (io/resource "day13_test_states.txt"))
       (string/split-lines)
       (remove empty?)
       (partition 6)
       (map #(string/join \newline %))
       (into [])))

(def first-turn :turn/left)

(def next-turn
  {:turn/left :turn/straight
   :turn/straight :turn/right
   :turn/right :turn/left})

(def cart-dirs
  {\< :dir/left
   \> :dir/right
   \^ :dir/up
   \v :dir/down})

(def opposite-dirs
  {:dir/left :dir/right
  :dir/right :dir/left 
  :dir/up :dir/down
  :dir/down :dir/up})

(defmulti move (fn [pos dir] dir))

(defmethod move :dir/up
  [[x y] _]
  [x (dec y)])

(defmethod move :dir/down
  [[x y] _]
  [x (inc y)])

(defmethod move :dir/left
  [[x y] _]
  [(dec x) y])

(defmethod move :dir/right
  [[x y] _]
  [(inc x) y])

(def track-pieces #{\\ \/ \- \| \+ \> \< \^ \v})

(defn add-track
  [g [x y :as p] section]
  (apply graph/add-edges g
         (case section
           \- [[p (move p :dir/left)] [p (move p :dir/right)]]
           \| [[p (move p :dir/up)] [p (move p :dir/down)]])))

(defn get-in-cp
  [cp [x y]]
  (get-in cp [y x]))

(defn add-corner
  [g [x y :as p] corner cart-map]
  (let [u (move p :dir/up)
        l (move p :dir/left) 
        d (move p :dir/down)
        r (move p :dir/right)]
    (cond
      ; -\  or |
      ;  |     \-
      ; left down or up right
      (and (= \\ corner)
           (track-pieces (get-in-cp cart-map u))
           (track-pieces (get-in-cp cart-map r))) (graph/add-edges g [p u] [p r])
      (and (= \\ corner)
           (track-pieces (get-in-cp cart-map l))
           (track-pieces (get-in-cp cart-map d))) (graph/add-edges g [p l] [p d])
      ; /-     |
      ; |     -/
      ; right down or up left
      (and (= \/ corner)
           (track-pieces (get-in-cp cart-map u))
           (track-pieces (get-in-cp cart-map l))) (graph/add-edges g [p u] [p l])
      (and (= \/ corner)
           (track-pieces (get-in-cp cart-map d))
           (track-pieces (get-in-cp cart-map r))) (graph/add-edges g [p d] [p r]))))

(defn add-inter
  [g [x y :as p] cart-map]
  (reduce (fn [g [nx ny :as np]]
            (if (contains? track-pieces (get-in cart-map [ny nx]))
              (graph/add-edges g [p np])
              g))
          g
          [(move p :dir/up) (move p :dir/left) (move p :dir/down) (move p :dir/down)]))

(def cart->track
  {\^ \|
   \v \|
   \< \-
   \> \-})

(defn add-cart
  [state [x y :as p] cart]
  ; add edge to graph 
  ; update cart map
  (-> state
      (update :graph add-track p (cart->track cart))
      (update :carts conj {:pos p
                           :dir (cart-dirs cart)
                           :last-turn :turn/right})))

#_(pprint (parse-state (first test-states)))
(defn parse-state
  [s]
  (let [lines (string/split-lines s)
        height (count lines)
        width (reduce #(max %1 (count %2)) 0 lines)
        cart-map (into [] (map vec) lines)]
    (reduce (fn [{:keys [carts graph] :as state} [x y :as p]]
              (let [c (get-in cart-map [y x])]
                (cond
                  (contains? #{\| \-} c) (update state :graph add-track p c)
                  (contains? #{\\ \/} c) (update state :graph add-corner p c cart-map)
                  (= c \+) (update state :graph add-inter p cart-map)
                  (contains? cart-dirs c) (add-cart state p c)
                  :else state)))
            {:carts #{} :graph (graph/graph)}
            (for [y (range height)
                  x (range width)]
              [x y]))))

(def test-nodes
  #{[0 0] [1 0] [2 0] [3 0] [4 0]
    [0 1]                   [4 1]             [7 1] [8 1] [9 1] [10 1] [11 1] [12 1]
    [0 2]       [2 2] [3 2] [4 2] [5 2] [6 2] [7 2] [8 2] [9 2]               [12 2]
    [0 3]       [2 3]       [4 3]             [7 3]       [9 3]               [12 3]
    [0 4] [1 4] [2 4] [3 4] [4 4]             [7 4] [8 4] [9 4] [10 4] [11 4] [12 4]
                [2 5] [3 5] [4 5] [5 5] [6 5] [7 5] [8 5] [9 5]})

(def test-adj-list
  {; right loop
   [7 1] #{[8 1] [7 2]}
   [8 1] #{[7 1] [9 1]}
   [9 1] #{[8 1] [10 1]}
   [10 1] #{[11 1] [9 1]}
   [11 1] #{[10 1] [12 1]}
   [12 1] #{[11 1] [12 2]}
   [12 2] #{[12 1] [12 3]}
   [12 3] #{[12 2] [12 4]}
   [12 4] #{[12 3] [11 4]}
   [11 4] #{[12 4] [10 4]}
   [10 4] #{[11 4] [9 4]}
   [9 4] #{[10 4] [9 3] [9 5] [8 4]}
   [8 4] #{[9 4] [7 4]}
   [7 4] #{[8 4] [7 3]}
   [7 3] #{[7 4] [7 2]}
   
   ; center loop
   [2 2] #{[3 2] [2 3]}
   [3 2] #{[4 2] [2 2]}
   [5 2] #{[4 2] [6 2]}
   [6 2] #{[5 2] [7 2]}
   [7 2] #{[7 1] [6 2] [8 2] [7 3]}
   [8 2] #{[7 2] [9 2]}
   [9 2] #{[8 2] [9 3]}
   [9 3] #{[9 2] [9 4]}
   [9 5] #{[9 4] [8 5]}
   [8 5] #{[7 5] [9 5]}
   [7 5] #{[8 5] [6 5]}
   [6 5] #{[5 5] [7 5]}
   [5 5] #{[4 5] [6 5]}
   [4 5] #{[3 5] [5 5]}
   [3 5] #{[4 5] [2 5]}
   [2 5] #{[2 4] [3 5]}
   [2 3] #{[2 4] [2 2]}

   ; left loop
   [0 0] #{[1 0] [0 1]}
   [1 0] #{[0 0] [2 0]}
   [2 0] #{[1 0] [3 0]}
   [3 0] #{[2 0] [4 0]}
   [4 0] #{[4 1] [3 0]}
   [4 1] #{[4 0] [4 2]}
   [4 2] #{[4 1] [4 3] [5 2] [3 2]}
   [4 3] #{[4 2] [4 4]}
   [4 4] #{[4 3] [3 4]}
   [3 4] #{[2 4] [4 4]}
   [2 4] #{[1 4] [3 4] [2 3] [2 5]}
   [1 4] #{[2 4] [0 4]}
   [0 4] #{[1 4] [0 3]}
   [0 3] #{[0 2] [0 4]}
   [0 2] #{[0 1] [0 3]}
   [0 1] #{[0 0] [0 2]}})

(deftest test-parse-state
  (let [{:keys [graph carts]} (parse-state (first test-states))
        carts-by-pos (zipmap (map :pos carts)
                             carts)]
    (testing "nodes"
      (= test-nodes (into #{} (graph/nodes graph))))
    (testing "edges"
      (doseq [n (graph/nodes graph)]
        (is (= (test-adj-list n)
               (set (graph/successors graph n))))))
    (testing "carts"
      (testing "cart at [2 0]"
        (let [{:keys [pos dir last-turn] :as cart} (carts-by-pos [2 0])]
          (is (some? cart))
          (is (= [2 0] pos))
          (is (= :dir/right dir))
          (is (= :turn/right last-turn))))
      (testing "cart at [9 3]"
        (let [{:keys [pos dir last-turn] :as cart} (carts-by-pos [9 3])]
          (is (some? cart))
          (is (= [9 3] pos))
          (is (= :dir/down dir))
          (is (= :turn/right last-turn)))))))


(defn at-corner?
  [{:keys [graph] :as state} {:keys [pos] :as cart}]
  (let [[[x0 y0] [x1 y1] :as ss] (seq (graph/successors graph pos))]
    (and (= 2 (count ss))
         ; not co-linear
         (not= x0 x1)
         (not= y0 y1))))


(defn at-intersection? 
  [{:keys [graph] :as state} {:keys [pos] :as cart}]
  (= 4 (count (graph/successors graph pos))))

(defn replace-cart 
  [state old-cart new-cart]
  (update state :carts #(-> % (disj old-cart) (conj new-cart))))


(defn move-cart-forward
  [{:keys [graph] :as state} {:keys [pos dir] :as cart}]
  (let [ss (graph/successors graph pos)
        next-pos (move pos dir)]
    (replace-cart state cart (assoc cart :pos next-pos))))


(defn dir-from
  [a b]
  (cond (= b (move a :dir/up)) :dir/up
        (= b (move a :dir/down)) :dir/down
        (= b (move a :dir/left)) :dir/left
        (= b (move a :dir/right)) :dir/right))


(defn turn-corner
  [{:keys [graph] :as state} {:keys [pos dir] :as cart}]
  (let [ss (disj (graph/successors graph pos)
                 (move pos (opposite-dirs dir)))
        next-pos (first ss)
        next-dir (dir-from pos next-pos)]
    (replace-cart state cart (assoc cart
                                    :pos next-pos
                                    :dir next-dir))))


(def dir-turns 
  {:dir/up [:dir/left :dir/right]
   :dir/right [:dir/up :dir/down]
   :dir/down [:dir/right :dir/left]
   :dir/left [:dir/down :dir/up]})

(defn dir-for-turn 
  [dir turn]
  (let [[left right] (dir-turns dir)]
    (case turn
      :turn/left left
      :turn/right right
      dir)))

(defn nav-intersection
  [{:keys [graph] :as state} {:keys [pos dir last-turn] :as cart}]
  (let [turn (next-turn last-turn)
        next-dir (dir-for-turn dir turn)
        next-pos (move pos next-dir)]
    (replace-cart state cart (assoc cart
                                    :pos next-pos
                                    :dir next-dir
                                    :last-turn turn))))

(defn move-cart
  [state {:keys [pos dir last-turn] :as cart}]
  ; move along track
  ; turn at a corner
  ; at intersection, turn based on last-turn 
  (cond (at-corner? state cart) (turn-corner state cart)
        (at-intersection? state cart) (nav-intersection state cart) 
        :else (move-cart-forward state cart)))


(defn sort-carts
  [carts]
  (sort-by :pos
           (fn [[x0 y0] [x1 y1]]
             (if (= y0 y1)
               (< x0 x1) 
               (< y0 y1)))
           carts))


(defn next-state
  [state]
  (reduce move-cart state (sort-carts (:carts state))))


(defn same-carts?
  [{s0 :carts} {s1 :carts}]
  (and (some? s0)
       (some? s1)
       (every? true? (map (fn [x y]
                            (= (:pos x) (:pos y)))
                          (sort-carts s0)
                          (sort-carts s1)))))



(deftest test-next-state
  (testing "test move 1 tick"
    (let [s0 (parse-state (first test-states))
          expected-s1  (parse-state (second test-states))
          s1 (next-state s0)]
      (is (same-carts? expected-s1 s1))))
  (testing "check move all test ticks"
    (loop [[expected-state & tss] (pop test-states)
           s_n (parse-state (first test-states))
           i 0]
      (when (some? expected-state)
        (is (same-carts? (parse-state expected-state) s_n) i)
        (recur tss (next-state s_n) (inc i))))))

(defn nth-state
  [initial n]
  (->> (iterate next-state initial)
       (drop n)
       first))


#_(print-states-at 13)
(defn print-states-at
  [i]
  (prn "expected" (->> (nth test-states i) (parse-state ) (:carts) (sort-carts)))
  (prn "actual" (:carts (nth-state (parse-state (first test-states)) i))))

(defn crash-pos 
  [{:keys [carts] :as state}]
  (some (fn [[pos carts]]
          (if (< 1 (count carts))
            pos))
        (group-by :pos carts)))

(deftest test-crash-pos
  (is (= [7 3]
         (crash-pos (nth-state (parse-state (first test-states)) 14)))))

(defn first-crash
  ([initial-state] (first-crash (iterate next-state initial-state) 0))
  ([[s & states] i]
   (if-let [pos (crash-pos s)]
     [pos i]
     (recur states (inc i)))))

#_(first-crash (parse-state (first test-states)))

(comment
  (def initial-state (parse-state (slurp (io/resource "day13.txt"))))

  (first-crash initial-state)

  (first (drop-while #(nil? (crash-pos %)) (iterate next-state initial-state)))
  ; NEGATIVE POSITIONS!!!!
  )
