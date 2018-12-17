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
       (map #(string/join \newline %))))


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

(defn find-carts
  [cart-map width height]
  (for [y (range height)
        x (range width)
        :let [cart-dir (get cart-dirs (get-in cart-map [y x]))]
        :when (some? cart-dir)]
    {:pos [x y]
     :dir cart-dir}))

(defn up
  [[x y]]
  [x (dec y)])

(defn down
  [[x y]]
  [x (inc y)])

(defn left
  [[x y]]
  [(dec x) y])

(defn right
  [[x y]]
  [(inc x) y])

(def track-pieces #{\\ \/ \- \| \+ \> \< \^ \v})

(defn add-track
  [g [x y :as p] section]
  (apply graph/add-edges g
         (case section
           \- [[p (left p)] [p (right p)]]
           \| [[p (up p)]
               [p (down p)]])))

(defn get-in-cp
  [cp [x y]]
  (get-in cp [y x]))

(defn add-corner
  [g [x y :as p] corner cart-map]
  (let [u (up p) l (left p) d (down p) r (right p)]
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
          [(up p) (left p) (down p) (right p)]))

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
      (update :cart conj {:pos p :dir (cart-dirs cart)})))

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
   [9 3] #{[9 2] [9 5]}
   [9 5] #{[9 4] [8 5]}
   [8 5] #{[7 5] [9 5]}
   [7 5] #{[8 5] [6 5]}
   [6 5] #{[5 5] [4 5]}
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
  (testing "nodes"
    (let [{:keys [graph]} (parse-state (first test-states))]
      (= test-nodes (into #{} (graph/nodes graph)))))
  (testing "edges"
    (doseq [n (graph/nodes)])

    ))



