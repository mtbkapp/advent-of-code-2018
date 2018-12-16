(ns advent-of-code-2018.day13
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [loom.graph :as graph]))


(println (first test-states))
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

(defn add-cart
  [state [x y] cart]
  ; add edge to graph 
  ; update cart map
  state
  )

(parse-state (first test-states))
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
            {:carts {}
             :graph (graph/graph)}
            (for [y (range height)
                  x (range width)]
              [x y]))))

