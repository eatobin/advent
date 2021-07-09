(ns advent.day12
  (:require
    [clojure.math.numeric-tower :as Math]))

(def state
  [{:pos [+3 -6 +6] :vel [0 0 0]}
   {:pos [+10 +7 -9] :vel [0 0 0]}
   {:pos [-3 -7 +9] :vel [0 0 0]}
   {:pos [-8 0 4] :vel [0 0 0]}])

(defn add [a b] (mapv + a b))

(defn step [state]
  (doall
    (for [m state
          :let [new-velocities
                (reduce
                  add
                  (:vel m)
                  (for [n state] (mapv compare (:pos n) (:pos m))))]]
      (-> m
          (assoc :vel new-velocities)
          (update :pos add new-velocities)))))

(defn norm [xs] (transduce (map #(Math/abs (double %))) + xs))

(defn total [moon] (* (norm (:vel moon)) (norm (:pos moon))))
(defn total-system [state] (reduce + (map total state)))

(println "Total energy:" (total-system (nth (iterate step state) 1000)))

(defn third [x] (nth x 2))
(defn gcd [a b] (if (zero? b) a (recur b, (mod a b))))
(defn lcm [& v] (reduce (fn [a b] (/ (* a b) (gcd a b))) v))

(defn first-repeat [step state proj-fn]
  (reduce (fn [acc state]
            (if (contains? acc state)
              (reduced (count acc))
              (conj acc state)))
          #{} (map proj-fn (iterate step state))))

(let [offsets (for [proj [first second third]]
                (first-repeat step state (partial mapv (juxt (comp proj :pos) (comp proj :vel)))))]
  (println "Second solution:" (apply lcm offsets)))
