(ns advent-of-code-2020.four
  (:require [clojure.string :as str]))

(def passport-fields #{:ecl :pid :eyr :hcl :byr :iyr :hgt})

(defn group-passports
  "Given a collection of lines
 group passports"
  [passports]
  (let [passport-count (atom 0)]
    (partition-by
      (fn [l]
        (when (str/blank? l)
          (swap! passport-count inc))
        @passport-count)
      passports)))

(defn vec->passport
  "Given a vector convert to passport map."
  [passport]
  (into {}
        (comp
          (filter (comp not str/blank?))
          (filter (comp not #(str/starts-with? % "cid")))
          (map (fn [f]
                 (let [[k v] (str/split f #":")]
                   [(keyword k) v]))))

        passport))

(defn passport-maps
  "Given a collection of passport lines
 convert them to maps."
  [passports]
  (->> passports
       (map (partial str/join " "))
       (map #(str/split % #" "))
       (map vec->passport)))

(defn valid-passport?
  "Given a passport map determine if passport keys are valid."
  [passport]
  (= passport-fields
     (set (keys passport))))

(defn strict-valid-passport?
  "Given a passport map determine if passport _values_ are valid."
  [passport]
  (let [{:keys [ecl pid eyr hcl byr iyr hgt]} passport
        byr? (and (= 4 (count byr))
                  (try (<= 1920 (Integer/parseInt byr) 2002)
                       (catch Exception _ nil)))
        iyr? (and (= 4 (count iyr))
                  (try (<= 2010 (Integer/parseInt iyr) 2020)
                       (catch Exception _ nil)))
        eyr? (and (= 4 (count eyr))
                  (try (<= 2020 (Integer/parseInt eyr) 2030)
                       (catch Exception _ nil)))
        hgt? (or (and (str/ends-with? hgt "cm")
                      (try (<= 150 (Integer/parseInt (str/join (drop-last 2 hgt))) 193)
                           (catch Exception _ nil)))
                 (and (str/ends-with? hgt "in")
                      (try (<= 59 (Integer/parseInt (str/join (drop-last 2 hgt))) 76)
                           (catch Exception _ nil))))
        hcl? (re-matches #"^#[0-9a-f]{6}" hcl)
        ecl? (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} ecl)
        pid? (re-matches #"^[0-9]{9}" pid)]
    (and byr? iyr? eyr? hgt? hcl? ecl? pid?)))

(comment
  (def input (slurp "./resources/four.txt"))

  (->> input
       str/split-lines
       group-passports
       passport-maps
       (filter valid-passport?)
       (filter strict-valid-passport?)
       count))
