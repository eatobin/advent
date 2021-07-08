;new text
(ns advent.day12
  (:require
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [clojure.string :as str]
    [clojure.math.numeric-tower :as math]))

;part a
(def answer (atom 0))

(def vcs (with-open [reader (io/reader "resources/day12.csv")]
           (doall
             (csv/read-csv reader))))

(defn make-moon-map [moon-pos]
  (->>
    moon-pos
    (map #(str/replace % ">" ""))
    (map #(str/split % #"="))
    (map #(get % 1))
    (map #(Integer/parseInt %))
    (zipmap [:x :y :z])))

(def moon-maps (into [] (map make-moon-map vcs)))

(def moon-template
  {:i {:name    :i,
       :pos     {:x 0, :y 0, :z 0}
       :vel     {:x 0, :y 0, :z 0}
       :pot     {:x 0, :y 0, :z 0}
       :kin     {:x 0, :y 0, :z 0}
       :tot-pot 0
       :tot-kin 0
       :tot-tot 0}
   :e {:name    :e,
       :pos     {:x 0, :y 0, :z 0}
       :vel     {:x 0, :y 0, :z 0}
       :pot     {:x 0, :y 0, :z 0}
       :kin     {:x 0, :y 0, :z 0}
       :tot-pot 0
       :tot-kin 0
       :tot-tot 0}
   :g {:name    :g,
       :pos     {:x 0, :y 0, :z 0}
       :vel     {:x 0, :y 0, :z 0}
       :pot     {:x 0, :y 0, :z 0}
       :kin     {:x 0, :y 0, :z 0}
       :tot-pot 0
       :tot-kin 0
       :tot-tot 0}
   :c {:name    :c,
       :pos     {:x 0, :y 0, :z 0}
       :vel     {:x 0, :y 0, :z 0}
       :pot     {:x 0, :y 0, :z 0}
       :kin     {:x 0, :y 0, :z 0}
       :tot-pot 0
       :tot-kin 0
       :tot-tot 0}})

(def moon-meld
  (atom (->
          moon-template
          (assoc-in [:i :pos] (get moon-maps 0))
          (assoc-in [:e :pos] (get moon-maps 1))
          (assoc-in [:g :pos] (get moon-maps 2))
          (assoc-in [:c :pos] (get moon-maps 3)))))

(def candidates
  [[[:i :pos :x] [:e :pos :x]]
   [[:i :pos :x] [:g :pos :x]]
   [[:i :pos :x] [:c :pos :x]]
   [[:e :pos :x] [:g :pos :x]]
   [[:e :pos :x] [:c :pos :x]]
   [[:g :pos :x] [:c :pos :x]]

   [[:i :pos :y] [:e :pos :y]]
   [[:i :pos :y] [:g :pos :y]]
   [[:i :pos :y] [:c :pos :y]]
   [[:e :pos :y] [:g :pos :y]]
   [[:e :pos :y] [:c :pos :y]]
   [[:g :pos :y] [:c :pos :y]]

   [[:i :pos :z] [:e :pos :z]]
   [[:i :pos :z] [:g :pos :z]]
   [[:i :pos :z] [:c :pos :z]]
   [[:e :pos :z] [:g :pos :z]]
   [[:e :pos :z] [:c :pos :z]]
   [[:g :pos :z] [:c :pos :z]]])

(defn moon-getter [moon]
  [(moon 0) (moon 2) (get-in @moon-meld moon)])

(defn moons-pair [moons]
  (vec (map moon-getter moons)))

(defn apply-gravity []
  (doseq [[moon-vec-0 moon-vec-1] (vec (map moons-pair candidates))
          :let [moon-pos-0 (get moon-vec-0 2)
                moon-pos-1 (get moon-vec-1 2)
                moon-0 (get moon-vec-0 0)
                moon-1 (get moon-vec-1 0)
                axis (get moon-vec-0 1)
                moon-0-velocity (cond (> moon-pos-0 moon-pos-1) -1
                                      (> moon-pos-1 moon-pos-0) 1
                                      :else 0)
                moon-1-velocity (cond (> moon-pos-1 moon-pos-0) -1
                                      (> moon-pos-0 moon-pos-1) 1
                                      :else 0)]]
    (swap! moon-meld update-in [moon-0 :vel axis] + moon-0-velocity)
    (swap! moon-meld update-in [moon-1 :vel axis] + moon-1-velocity)))

(defn apply-velocity []
  (doseq [[_ {name :name, {x-vel :x, y-vel :y, z-vel :z} :vel}] @moon-meld]
    (swap! moon-meld update-in [name :pos :x] + x-vel)
    (swap! moon-meld update-in [name :pos :y] + y-vel)
    (swap! moon-meld update-in [name :pos :z] + z-vel)))

(defn calc-pe []
  (doseq [[_ {name :name, {x-pos :x, y-pos :y, z-pos :z} :pos}] @moon-meld]
    (swap! moon-meld assoc-in [name :pot :x] (math/abs x-pos))
    (swap! moon-meld assoc-in [name :pot :y] (math/abs y-pos))
    (swap! moon-meld assoc-in [name :pot :z] (math/abs z-pos))))

(defn calc-ke []
  (doseq [[_ {name :name, {x-vel :x, y-vel :y, z-vel :z} :vel}] @moon-meld]
    (swap! moon-meld assoc-in [name :kin :x] (math/abs x-vel))
    (swap! moon-meld assoc-in [name :kin :y] (math/abs y-vel))
    (swap! moon-meld assoc-in [name :kin :z] (math/abs z-vel))))

(defn tot-pot []
  (doseq [[_ {name :name, {x-pot :x, y-pot :y, z-pot :z} :pot}] @moon-meld]
    (swap! moon-meld assoc-in [name :tot-pot] (+ x-pot y-pot z-pot))))

(defn tot-kin []
  (doseq [[_ {name :name, {x-kin :x, y-kin :y, z-kin :z} :kin}] @moon-meld]
    (swap! moon-meld assoc-in [name :tot-kin] (+ x-kin y-kin z-kin))))

(defn tot-tot []
  (doseq [[_ {name :name, tot-pot :tot-pot, tot-kin :tot-kin}] @moon-meld]
    (swap! moon-meld assoc-in [name :tot-tot] (* tot-pot tot-kin))))

(def one-thousand-step-vec
  (vec
    (flatten
      (take 1000
            (repeat
              (interleave [apply-gravity] [apply-velocity] [calc-pe] [calc-ke] [tot-pot] [tot-kin] [tot-tot]))))))

(defn one-thousand-step []
  (doall (map #(%) one-thousand-step-vec)))

(defn calc-answer []
  (doseq [[_ {tot-tot :tot-tot}] @moon-meld]
    (swap! answer + tot-tot)))

(defn step-n-calc []
  (doall (map #(%) [one-thousand-step calc-answer])))

(step-n-calc)

(println @answer)
