(ns advent-2019.one
  (:require [clojure.string :as str]))

(defn read-input []
  (map #(Integer/parseInt %) (str/split (slurp "resources/inputs/one.txt") #"\n")))

(defn fuel-required [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn solution-1 []
  (reduce + (map fuel-required (read-input))))

(defn total-fuel-required [mass]
  (let [[_mass & fuels-required] (take-while pos? (iterate fuel-required mass))]
    (reduce + fuels-required)))

(defn solution-2 []
  (reduce + (map total-fuel-required (read-input))))
