(ns advent-of-code-2020.nine
  (:require
    [clojure.string :as str]))

(defn two-sum
  "Find given a list of integers, find two numbers that add up to `target-sum`."
  [target-sum input]
  (let [sorted-input (sort input)]
    (loop [head  (first sorted-input)
           tail  (last sorted-input)
           input sorted-input]
      (if (or (nil? head) (nil? tail))
        false
        (let [current-sum (+ head tail)]
          (if (= target-sum
                 current-sum)
            true
            (cond
              (> target-sum current-sum) (recur (second input) tail (rest input))
              (< target-sum current-sum) (recur head (last (butlast input)) (butlast input))
              :else false)))))))

(defn parse-input
  "Given a resource parse the input into a list of numbers."
  [resource-path]
  (->> resource-path
       slurp
       str/split-lines
       (map #(try (Integer/parseInt %) (catch Exception _ 0)))))

(defn invalid-number
  "Find the invalid number in the series."
  [series preamble-length]
  (loop [series series]
    (let [preamble (take preamble-length series)
          n        (first (drop preamble-length series))]
      (if-not (two-sum n preamble)
        n
        (recur (rest series))))))

(defn find-weakness
  "Find the weakness of the series"
  [series invalid-number]
  (loop [l      2
         offset 0]
    (let [weakness-list (->> series
                             (drop offset)
                             (take l))
          sum           (apply + weakness-list)]
      (cond
        (= invalid-number sum) (+ (apply max weakness-list) (apply min weakness-list))
        (> invalid-number sum) (recur (inc l) offset)
        (< invalid-number sum) (recur (dec l) (inc offset))))))

(comment
  (invalid-number (parse-input "./resources/nine.txt") 25)
  (find-weakness (parse-input "./resources/nine.txt") 22477624))




