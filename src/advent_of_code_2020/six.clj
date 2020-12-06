(ns advent-of-code-2020.six
  (:require
    [clojure.string :as str]
    [clojure.set :refer [intersection]]))

(comment
  (def input
    (let [group-count (atom 0)]
      (->> "./resources/six.txt"
           slurp
           str/split-lines
           (group-by
             (fn [l]
               (if (str/blank? l)
                 (swap! group-count inc))
               @group-count))
           vals)))

  (->> (second input)
       (filter (comp not str/blank?))
       count)

  (reduce
    (fn [questions next-person]
      (intersection questions
                    (into #{} next-person)))
    (into #{} (ffirst input))
    (rest (first input)))

  ;; part 1
  (->> input
       (map
         #(reduce
            (fn [questions next-person]
              (into questions next-person))
            #{}
            %))
       (map count)
       (apply +))

  ;; part 2
  (->> input
       (map
         (partial remove str/blank?))
       (map
         #(reduce
            (fn [questions next-person]
              (intersection questions
                            (into #{} next-person)))
            (into #{} (first %))
            (rest %)))
       (map count)
       (apply +)))
