(ns advent-of-code-2018.day13
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.set :as sets]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [loom.io :as lio]
            [loom.graph :as graph])
  (:gen-class))


#_(println (first test-states))
(def test-states
  (->> (slurp (io/resource "day13_test_states.txt"))
       (string/split-lines)
       (remove empty?)
       (partition 6)
       (map #(string/join \newline %))
       (into [])))


(def test-states2
  (->> (slurp (io/resource "day13_crashing.txt"))
       (string/split-lines)
       (remove empty?)
       (partition 7)
       (map #(string/join \newline %))
       (into [])))


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

(def cart-chars #{\^ \v \< \>})


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
  (let [[up lp dp rp :as dirs] (map (partial move p)
                                    [:dir/up :dir/left :dir/down :dir/right])
        [uc lc dc rc :as chrs] (map (partial get-in-cp cart-map) dirs)
        h-tracks (into #{\- \+} cart-chars)
        v-tracks (into #{\| \+} cart-chars)]
    (cond
      ; -\
      ;  |
      (and (= \\ corner)
           (h-tracks lc)
           (v-tracks dc))
      (graph/add-edges g [p lp] [p dp])
      ; |
      ; \-
      (and (= \\ corner)
           (h-tracks rc)
           (v-tracks uc))
      (graph/add-edges g [p rp] [p up])
      ; /-
      ; |
      (and (= \/ corner)
           (h-tracks rc)
           (v-tracks dc))
      (graph/add-edges g [p rp] [p dp])
      ;  |
      ; -/
      (and (= \/ corner)
           (h-tracks lc)
           (v-tracks uc))
      (graph/add-edges g [p lp] [p up])
      :else
      (throw (ex-info "corner not a corner" {:p p :corner corner :chars chrs})))))


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


(def color-reset "\u001B[0m")
(def color-cart "\u001B[46m")

#_(print-state (parse-state (slurp (io/resource "day13_corner_case.txt"))))
(defn print-state
  [{:keys [graph carts]}]
  (let [nodes (graph/nodes graph)
        [max-x max-y] (reduce (fn [[max-x max-y] [x y]]
                                [(max max-x x) (max max-y y)])
                              [0 0] nodes)
        carts-by-pos (reduce (fn [idx {:keys [pos] :as c}]
                               (assoc idx pos c))
                             {} carts)]
    (doseq [y (range 0 (inc max-y))]
      (doseq [x (range 0 (inc max-x))]
        (print (cond (contains? carts-by-pos [x y])
                     (str color-cart
                          (case (:dir (carts-by-pos [x y]))
                            :dir/left \<
                            :dir/right \>
                            :dir/up \^
                            :dir/down \v)
                          color-reset)
                     (contains? nodes [x y])
                     (let [[[x0 y0 :as p0] [x1 y1 :as p1] :as ss]
                           (sort-by identity (comparator (fn [[x0 y0] [x1 y1]]
                                                           (if (= y0 y1)
                                                             (< x0 x1)
                                                             (< y0 y1))))
                                    (graph/successors graph [x y]))]
                       (cond ;intersection
                             (= 4 (count ss)) \+
                             ; straight
                             (= y0 y1) \-
                             (= x0 x1) \|
                             ; corners
                             ; |
                             ; \-
                             (and (= x0 x) (= y0 (dec y)) 
                                  (= x1 (inc x)) (= y1 y)) \\
                             ; -\
                             ;  |
                             (and (= x0 (dec x)) (= y0 y)
                                  (= x1 x) (= y1 (inc y))) \\
                             ; /-
                             ; |
                             (and (= x0 (inc x)) (= y0 y)
                                  (= x1 x) (= y1 (inc y))) \/
                             ; |
                             ;-/
                             (and (= x0 x) (= y0 (dec y))
                                  (= x1 (dec x)) (= y1 y)) \/
                             :else \E))
                     :else \space)))
      (println))))


(deftest test-print-state
  (let [state-string (slurp (io/resource "day13_corner_case.txt"))
        state (parse-state state-string)
        state-lines (into []
                          (map string/trim)
                          (string/split-lines state-string))
        printed-lines (into []
                            (map string/trim)
                            (string/split-lines (with-out-str (print-state state))))]
    (is (= (count state-lines) (count printed-lines)))
    (doseq [i (range (count state-lines))]
      (is (= (nth state-lines i)
             (nth printed-lines i))))))


(def corner-test-nodes
  #{[0 0] [1 0] [2 0] [3 0]
    [0 1]       [2 1] [3 1] [4 1] [5 1]
    [0 2] [1 2] [2 2] [3 2] [4 2] [5 2] [6 2] [7 2]
    [1 3] [2 3] [3 3] [4 3] [5 3] [6 3] [7 3] [8 3]
    [1 4] [2 4] [3 4] [4 4] [5 4]       [7 4] [8 4]
    [1 5]             [4 5] [5 5] [6 5] [7 5] [8 5]
    [1 6] [2 6] [3 6] [4 6] [5 6] [6 6] [7 6] [8 6]})

(def corner-test-adj-list
  {[0 0] #{[0 1] [1 0]}
   [1 0] #{[0 0] [2 0]}
   [2 0] #{[1 0] [3 0]}
   [3 0] #{[2 0] [3 1]}
   [0 1] #{[0 0] [0 2]}
   [2 1] #{[2 2] [3 1]}
   [3 1] #{[3 0] [3 2] [2 1] [4 1]}
   [4 1] #{[3 1] [5 1]}
   [5 1] #{[4 1] [5 2]}
   [0 2] #{[0 1] [1 2]}
   [1 2] #{[0 2] [2 2]}
   [2 2] #{[1 2] [2 1] [2 3] [3 2]}
   [3 2] #{[3 1] [2 2]}
   [4 2] #{[4 3] [5 2]}
   [5 2] #{[4 2] [5 1] [6 2] [5 3]}
   [6 2] #{[5 2] [7 2]}
   [7 2] #{[7 3] [6 2]}
   [1 3] #{[2 3] [1 4]}
   [2 3] #{[1 3] [2 2] [3 3] [2 4]}
   [3 3] #{[2 3] [4 3]}
   [4 3] #{[3 3] [4 2] [4 4] [5 3]}
   [5 3] #{[4 3] [5 2] [6 3] [5 4]}
   [6 3] #{[5 3] [7 3]}
   [7 3] #{[7 2] [6 3] [7 4] [8 3]}
   [8 3] #{[7 3] [8 4]}
   [1 4] #{[1 3] [1 5]}
   [2 4] #{[2 3] [3 4]}
   [3 4] #{[2 4] [4 4]}
   [4 4] #{[3 4] [5 4] [4 3] [4 5]}
   [5 4] #{[4 4] [5 3]}
   [7 4] #{[7 3] [7 5]}
   [8 4] #{[8 3] [8 5]}
   [1 5] #{[1 4] [1 6]}
   [4 5] #{[4 4] [5 5]}
   [5 5] #{[4 5] [6 5]}
   [6 5] #{[5 5] [7 5]}
   [7 5] #{[7 4] [6 5]}
   [8 5] #{[8 4] [8 6]}
   [1 6] #{[1 5] [2 6]}
   [2 6] #{[1 6] [3 6]}
   [3 6] #{[2 6] [4 6]}
   [4 6] #{[3 6] [5 6]}
   [5 6] #{[4 6] [6 6]}
   [6 6] #{[5 6] [7 6]}
   [7 6] #{[6 6] [8 6]}
   [8 6] #{[8 5] [7 6]}})

(deftest test-corner-test
  (let [{:keys [graph]} (parse-state (slurp (io/resource "day13_corner_case.txt")))]
    (testing "nodes"
      (is (= corner-test-nodes (graph/nodes graph))))
    (testing "neighbors"
      (doseq [n (graph/nodes graph)]
        (is (= (corner-test-adj-list n)
               (set (graph/successors graph n)))
            n)))))

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
  (cond (at-corner? state cart) (turn-corner state cart)
        (at-intersection? state cart) (nav-intersection state cart) 
        :else (move-cart-forward state cart)))


(def compare-carts
  (comparator (fn [{[x0 y0] :pos} {[x1 y1] :pos}]
                (if (= y0 y1)
                  (< x0 x1) 
                  (< y0 y1)))))


(defn crashed-carts
  [{:keys [carts] :as state}]
  (into #{} (comp (map val)
                  (filter #(< 1 (count %)))
                  (mapcat identity))
        (group-by :pos carts)))


(defn remove-carts
  [state carts]
  (reduce (fn [s c]
            (update s :carts disj c))
          state
          carts))


#_(next-state (parse-state (slurp (io/resource "day13_corner_case.txt"))))
(defn next-state
  [{:keys [carts] :as state}]
  (loop [s0 state 
         [c & cs] carts
         crashed #{}]
    (if (some? c)
      (if (contains? crashed c)
        (recur s0 cs crashed)
        (let [s1 (move-cart s0 c)
              new-crashed (crashed-carts s1)]
          (recur (remove-carts s1 new-crashed)
                 cs
                 (into crashed new-crashed))))
      s0)))


(defn same-carts?
  [{s0 :carts} {s1 :carts}]
  (and (some? s0)
       (some? s1)
       (every? true? (map (fn [x y]
                            (= (:pos x) (:pos y)))
                          (sort compare-carts s0)
                          (sort compare-carts s1)))))


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

(defn first-state-with-1-cart
  [initial-state]
  (first (drop-while (fn [{:keys [carts] :as state}]
                       (< 1 (count carts)))
                     (iterate next-state initial-state))))


(comment
  (first (drop-while (fn [{:keys [carts] :as state}]
                       (< 1 (count carts)))
                     (iterate next-state (parse-state (slurp (io/resource "day13.txt"))))))

  (print-state
    (first-state-with-1-cart
      (parse-state (first test-states2))))

  (-> (first-state-with-1-cart
        (parse-state (first test-states2)))
      :carts)

  ;wrong guess 36,131
  (-> (first-state-with-1-cart
        (parse-state (slurp (io/resource "day13.txt"))))
      :carts)

  (def initial (parse-state (slurp (io/resource "day13.txt"))))



  
  )


(defn -main
  [& args]
  (loop [s (parse-state (first test-states2))
         i 0]
    (println "State #" i)
    (print-state s)
    (prn (:carts s))
    (.readLine *in*)
    (println "Press Enter")
    (recur (next-state s) (inc i))))
