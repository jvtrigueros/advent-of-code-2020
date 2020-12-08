(ns advent-of-code-2020.eight
  (:require [clojure.string :as str]))

(defn parse-input
  "Given a resource string, create consumable input."
  [resource-path]
  (->> resource-path
       slurp
       str/split-lines
       (mapv #(str/split % #" "))
       (mapv (fn [[inst offset]]
               [(keyword inst)
                (try (Integer/parseInt offset) (catch Exception _ 0))]))))

(defn acc-before-loop
  "Get accumulator before the infinite loop"
  [instructions]
  (reduce
    (fn [[pos visited acc] _]
      (if (contains? visited pos)
        (reduced [pos acc])
        (let [[op offset] (get instructions pos)]
          (case op
            :nop [(inc pos) (conj visited pos) acc]
            :acc [(inc pos) (conj visited pos) (+ acc offset)]
            :jmp [(+ pos offset) (conj visited pos) acc]
            (reduced [pos acc])))))
    [0 #{} 0]
    (range (count instructions))))

(defn find-nop-jmp
  "Find all the nops and jmps"
  [instructions]
  (keep-indexed
    (fn [pos [op offset]]
      (case op
        :jmp [pos [:nop offset]]
        :nop [pos [:jmp offset]]
        nil))
    instructions))

(defn run-with-retry
  "Re-run program flipping jmps -> nops and nops -> jmps."
  [instructions]
  (for [mod (find-nop-jmp instructions)
        :let [[pos instruction] mod
              instructions (assoc instructions pos instruction)
              [pos acc] (acc-before-loop instructions)]
        :when (= pos (count instructions))]
    acc))

(comment
  (let [instructions (parse-input "./resources/eight.txt")]
    (run-with-retry instructions)))



