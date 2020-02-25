(ns advent.day5
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;part a
(def tv (->> (first (with-open [reader (io/reader "resources/day5.csv")]
                      (doall
                        (csv/read-csv reader))))
             (map #(Integer/parseInt %))
             (into [])))

(defn pad-5 [n]
  (zipmap [:a :b :c :d :e]
          (for [n (format "%05d" n)]
            (- (byte n) 48))))

; y1
(defn a-p-w [pointer memory]
  (memory (+ 3 pointer)))

; y2
(defn b-p-r [pointer memory relative-base]
  (get memory (+ (memory (+ 2 pointer)) relative-base) 0))

; y3
(defn c-p-r [pointer memory relative-base]
  (get memory (+ (memory (+ 1 pointer)) relative-base) 0))

; y4, y6
(defn c-p-w-c-i-r [pointer memory]
  (memory (+ 1 pointer)))

; y5
(defn b-i-r [pointer memory]
  (memory (+ 2 pointer)))

(defn param-maker-c [instruction pointer memory]
  (case (instruction :e)
    1 (case (instruction :c)
        0 (c-p-r pointer memory 0)
        1 (c-p-w-c-i-r pointer memory))
    2 (case (instruction :c)
        0 (c-p-r pointer memory 0)
        1 (c-p-w-c-i-r pointer memory))
    3 (c-p-w-c-i-r pointer memory)
    4 (c-p-r pointer memory 0)
    5 (case (instruction :c)
        0 (c-p-r pointer memory 0)
        1 (c-p-w-c-i-r pointer memory))
    6 (case (instruction :c)
        0 (c-p-r pointer memory 0)
        1 (c-p-w-c-i-r pointer memory))
    7 (case (instruction :c)
        0 (c-p-r pointer memory 0)
        1 (c-p-w-c-i-r pointer memory))
    8 (case (instruction :c)
        0 (c-p-r pointer memory 0)
        1 (c-p-w-c-i-r pointer memory))))

(defn param-maker-b [instruction pointer memory]
  (case (instruction :e)
    1 (case (instruction :b)
        0 (b-p-r pointer memory 0)
        1 (b-i-r pointer memory))
    2 (case (instruction :b)
        0 (b-p-r pointer memory 0)
        1 (b-i-r pointer memory))
    5 (case (instruction :b)
        0 (b-p-r pointer memory 0)
        1 (b-i-r pointer memory))
    6 (case (instruction :b)
        0 (b-p-r pointer memory 0)
        1 (b-i-r pointer memory))
    7 (case (instruction :b)
        0 (b-p-r pointer memory 0)
        1 (b-i-r pointer memory))
    8 (case (instruction :b)
        0 (b-p-r pointer memory 0)
        1 (b-i-r pointer memory))))

(defn param-maker-a [instruction pointer memory]
  (case (instruction :e)
    1 (case (instruction :a)
        0 (a-p-w pointer memory))
    2 (case (instruction :a)
        0 (a-p-w pointer memory))
    7 (case (instruction :a)
        0 (a-p-w pointer memory))
    8 (case (instruction :a)
        0 (a-p-w pointer memory))))

(defn op-code [[input pointer memory]]
  (let [instruction (pad-5 (memory pointer))]
    (case (instruction :e)
      9 (if (= (instruction :d) 9)
          [input pointer memory]
          "not used")
      1 (recur
          [input
           (+ 4 pointer)
           (assoc memory (param-maker-a instruction pointer memory)
                         (+ (param-maker-c instruction pointer memory)
                            (param-maker-b instruction pointer memory)))])
      2 (recur
          [input
           (+ 4 pointer)
           (assoc memory (param-maker-a instruction pointer memory)
                         (* (param-maker-c instruction pointer memory)
                            (param-maker-b instruction pointer memory)))])
      3 (recur
          [input
           (+ 2 pointer)
           (assoc memory (param-maker-c instruction pointer memory) input)])
      4 (recur
          [(param-maker-c instruction pointer memory)
           (+ 2 pointer)
           memory]))))

(def answer (first (op-code [1 0 tv])))

(println answer) answer

;9025675

;part b

(defn op-code-2 [[input pointer memory]]
  (let [instruction (pad-5 (memory pointer))]
    (case (instruction :e)
      9 (if (= (instruction :d) 9)
          [input pointer memory]
          "not used")
      1 (recur
          [input
           (+ 4 pointer)
           (assoc memory (param-maker-a instruction pointer memory)
                         (+ (param-maker-c instruction pointer memory)
                            (param-maker-b instruction pointer memory)))])
      2 (recur
          [input
           (+ 4 pointer)
           (assoc memory (param-maker-a instruction pointer memory)
                         (* (param-maker-c instruction pointer memory)
                            (param-maker-b instruction pointer memory)))])
      3 (recur
          [input
           (+ 2 pointer)
           (assoc memory (param-maker-c instruction pointer memory) input)])
      4 (recur
          [(param-maker-c instruction pointer memory)
           (+ 2 pointer)
           memory])
      5 (recur
          [input
           (if (= 0 (param-maker-c instruction pointer memory))
             (+ 3 pointer)
             (param-maker-b instruction pointer memory))
           memory])
      6 (recur
          [input
           (if (not= 0 (param-maker-c instruction pointer memory))
             (+ 3 pointer)
             (param-maker-b instruction pointer memory))
           memory])
      7 (recur
          [input
           (+ 4 pointer)
           (if (< (param-maker-c instruction pointer memory) (param-maker-b instruction pointer memory))
             (assoc memory (param-maker-a instruction pointer memory) 1)
             (assoc memory (param-maker-a instruction pointer memory) 0))])
      8 (recur
          [input
           (+ 4 pointer)
           (if (= (param-maker-c instruction pointer memory) (param-maker-b instruction pointer memory))
             (assoc memory (param-maker-a instruction pointer memory) 1)
             (assoc memory (param-maker-a instruction pointer memory) 0))]))))

(def answer-2 (first (op-code-2 [5 0 tv])))

(println answer-2)

;11981754
