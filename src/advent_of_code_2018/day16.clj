(ns advent-of-code-2018.day16
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [clojure.test :refer :all]))


(spec/def ::registers (spec/tuple int? int? int? int?))

(spec/def ::instruction (spec/cat :optcode (spec/int-in 0 16)
                                  :input-a int?
                                  :input-b int?
                                  :output int?))


;; PARSE THE INPUT

(defn read-instr
  [line]
  (read-string (str "[" line "]")))

(defn parse-sample
  [[before-str instr-str after-str]]
  (let [read-state #(read-string (second (string/split % #":\s+")))]
    {:before (read-state before-str)
     :instr (read-instr instr-str) 
     :after (read-state after-str)}))

(def split-input
  (->> (slurp (io/resource "day16.txt"))
       (string/split-lines)
       (partition-by empty?)
       (remove #(every? empty? %))
       (reverse)))

#_(first split-input)
#_(count split-input)

(def sample-program
  (map read-instr (first split-input)))

#_(take 20 sample-program)

(def samples
  (map parse-sample (rest split-input)))

#_(take 20 samples)


;; IMPLEMENT THE DEVICE

(def instructions #{:addr :addi
                    :mulr :muli
                    :banr :bani
                    :borr :bori
                    :setr :seti
                    :gtir :gtri :gtrr
                    :eqir :eqri :eqrr})


(defn exec-mathr
  [registers [_ input-a input-b output] f]
  (assoc registers
         output
         (f (nth registers input-a)
            (nth registers input-b))))

(defn exec-mathi
  [registers [_ input-a input-b output] f]
  (assoc registers
         output
         (f (nth registers input-a) input-b)))



(defmulti exec (fn [registers [op & _]] op))

(defmethod exec :addr
  [registers instr]
  (exec-mathr registers instr +))

(deftest test-addr
  (is (= [19 9 8 7] (exec [10 9 8 7] [:addr 0 1 0])))
  (is (= [10 19 8 7] (exec [10 9 8 7] [:addr 0 1 1])))
  (is (= [10 9 19 7] (exec [10 9 8 7] [:addr 0 1 2])))
  (is (= [10 9 8 19] (exec [10 9 8 7] [:addr 0 1 3]))))

(defmethod exec :addi
  [registers instr]
  (exec-mathi registers instr +))

(deftest test-addi
  (is (= [300 0 0 0] (exec [200 0 0 0] [:addi 0 100 0])))
  (is (= [200 300 0 0] (exec [200 0 0 0] [:addi 0 100 1])))
  (is (= [200 0 300 0] (exec [200 0 0 0] [:addi 0 100 2])))
  (is (= [200 0 0 300] (exec [200 0 0 0] [:addi 0 100 3]))))



(defmethod exec :mulr
  [registers instr]
  (exec-mathr registers instr *))

(deftest test-mulr
  (is (= [90 9 8 7] (exec [10 9 8 7] [:mulr 0 1 0]))))



(defmethod exec :muli
  [registers instr]
  (exec-mathi registers instr *))

(deftest test-muli
  (is (= [20000 0 0 0] (exec [200 0 0 0] [:muli 0 100 0]))))




(defmethod exec :banr
  [registers instr]
  (exec-mathr registers instr bit-and))

(deftest test-banr
  (is (= [1 1 0 0] (exec [3 1 0 0] [:banr 0 1 0]))))



(defmethod exec :bani
  [registers instr]
  (exec-mathi registers instr bit-and))

(deftest test-bani
  (is (= [3 1 1 0] (exec [3 1 0 0] [:bani 0 1 2]))))




(defmethod exec :borr
  [registers instr]
  (exec-mathr registers instr bit-or))

(deftest test-borr
  (is (= [5 1 0 0] (exec [4 1 0 0] [:borr 0 1 0]))))



(defmethod exec :bori
  [registers instr]
  (exec-mathi registers instr bit-or))

(deftest test-bori
  (is (= [4 1 5 0] (exec [4 1 0 0] [:bori 0 1 2]))))



(defmethod exec :setr
  [registers [_ input-a input-b output]]
  (assoc registers output (nth registers input-a)))

(deftest test-setr
  (is (= [10 10 9 9] (exec [10 9 9 9] [:setr 0 9999 1]))))




(defmethod exec :seti
  [registers [_ input-a input-b output]]
  (assoc registers output input-a))

(deftest test-seti
  (is (= [777 9 9 9] (exec [10 9 9 9] [:seti 777 9999 0]))))




(defn exec-compare
  [registers [_ input-a input-b output] cf af bf]
  (assoc registers
         output
         (if (cf (af registers input-a)
                 (bf registers input-b)) 1 0)))

(defn get-imm
  [registers rn]
  rn)

(defn get-r
  [registers rn]
  (nth registers rn))

(defmethod exec :gtir
  [rs i]
  (exec-compare rs i > get-imm get-r))

(deftest test-gtir
  (is (= [10 9 8 1] (exec [10 9 8 7] [:gtir 33 3 3])))
  (is (= [10 9 8 0] (exec [10 9 8 7] [:gtir 2 3 3]))))



(defmethod exec :gtri
  [rs is]
  (exec-compare rs is > get-r get-imm))

(deftest test-gtri
  (is (= [10 9 8 0] (exec [10 9 8 7] [:gtri 0 33 3])))
  (is (= [10 9 8 1] (exec [10 9 8 7] [:gtri 0 2 3]))))



(defmethod exec :gtrr
  [rs is]
  (exec-compare rs is > get-r get-r))

(deftest test-gtrr
  (is (= [1 99 8 7] (exec [10 99 8 7] [:gtrr 1 2 0])))
  (is (= [0 8 99 7] (exec [10 8 99 7] [:gtrr 1 2 0]))))




(defmethod exec :eqir
  [rs is]
  (exec-compare rs is = get-imm get-r))

(deftest test-eqir
  (is (= [1 99 8 7] (exec [10 99 8 7] [:eqir 8 2 0])))
  (is (= [0 8 99 7] (exec [10 8 99 7] [:eqir 1 2 0]))))


(defmethod exec :eqri
  [rs is]
  (exec-compare rs is = get-r get-imm))

(deftest test-eqri
  (is (= [1 99 8 7] (exec [10 99 8 7] [:eqri 1 99 0])))
  (is (= [0 8 99 7] (exec [10 8 99 7] [:eqri 1 2 0]))))


(defmethod exec :eqrr
  [rs is]
  (exec-compare rs is = get-r get-r))

(deftest test-eqrr
  (is (= [1 99 99 7] (exec [10 99 99 7] [:eqrr 1 2 0])))
  (is (= [0 8 99 7] (exec [10 8 99 7] [:eqrr 1 2 0]))))


;; PART 1
(defn potential-instructions
  [{:keys [before after] [op-num & args] :instr}]
  (into #{}
        (filter #(= after (exec before (cons % args))))
        instructions))

(def test-sample
  (parse-sample ["Before: [3, 2, 1, 1]"
                 "9 2 1 2"
                 "After:  [3, 2, 2, 1]"]))

(deftest test-potential-instructions
  (is (= #{:mulr :addi :seti}
         (potential-instructions test-sample))))

(prn part1)
(def part1
  (->> samples
       (map (comp count potential-instructions)))
  )


