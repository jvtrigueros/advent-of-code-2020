(ns advent-of-code-2020.three
  (:require [clojure.string :as str]))

(defn make-tree-path
  "Given a forest, generate the tree path following the right 3, down 1 slope."
  [[r d] forest]
  (last
    (reduce
      (fn [[row path] tree-pattern]
        [(inc row)
         (do
           (println row d (zero? (mod row d)))
           (if (zero? (mod row d))
             (->> tree-pattern
                  (take (+ r (* row r)))
                  (drop (* row r))
                  (concat path))
             (do
               (let [t (->> tree-pattern
                            (take (+ 1 (* row r)))
                            (drop (* row r)))]
                 (println t)
                 (concat path t)))))])
      [0 (list)]
      forest)))

(defn count-trees
  "Count the number of trees encountered on a forest with a given slope of r(ight) and d(own)."
  [[r d] forest]
  (->> forest
       (make-tree-path [r d])
       (take-nth (+ r d))
       (filter #(= \# %))
       count))

(comment
 (def forest (mapv #(apply concat (repeat %))
                   (str/split-lines (slurp "./resources/three-1.txt"))))
 (def slopes [[1 1]
              [3 1]
              [5 1]
              [7 1]
              [1 2]])

 (apply * (map #(count-trees % forest) slopes))

 (make-tree-path [1 2] forest))
