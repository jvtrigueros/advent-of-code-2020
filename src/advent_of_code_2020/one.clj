(ns advent-of-code-2020.one)

(def ^:dynamic *input*)

(def target-sum 2020)

(comment
  (def website-input
    (slurp "./resources/one.txt"))
  (def ez-input
    [1721 979 366 299 675 1456]))

(defn two-sum-product
  "Find given a list of integers, find two numbers that add up to `target-sum` then return it's product."
  [target-sum input]
  (let [sorted-input (sort input)]
    (loop [head (first sorted-input)
           tail (last sorted-input)
           input sorted-input]
      (let [current-sum (+ head tail)]
        (if (= target-sum
               current-sum)
          (* head tail)
          (cond
            (> target-sum current-sum) (recur (second input) tail (rest input))
            (< target-sum current-sum) (recur head (last (butlast input)) (butlast input))
            :else 0))))))

(defn three-sum-product
  "Given a list of integers, find three numbers that add up to `target-sum` then return their product."
  [target-sum input]
  (let [sorted-input (sort input)]
    #_(map #(two-sum-product))))


(comment
  (require '[clojure.string :as str])
  (two-sum-product target-sum
                   (->> "./resources/one.txt"
                        slurp
                        str/split-lines
                        (mapv #(Integer/parseInt %)))))
