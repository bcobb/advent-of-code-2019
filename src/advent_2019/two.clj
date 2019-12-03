(ns advent-2019.two
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn read-input []
  (mapv
   #(Integer/parseInt %)
   (str/split (slurp "resources/inputs/two.txt") #",")))

(defn restore [program noun verb]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)))

(defn program->operations [program]
  (vec (map vec (partition 4 4 '() program))))

(defn apply-operation* [operation program [input1-pos input2-pos output-pos]]
  (let [input1 (nth program input1-pos)
        input2 (nth program input2-pos)]
    (assoc program output-pos (operation input1 input2))))

(defn run-program [program]
  (let [operations (program->operations program)]
    (loop [p program
           o (first operations)
           os (rest operations)]
      (let [[opcode & positions] o]
        (case opcode
          1 (recur (apply-operation* + p positions)
                   (first os)
                   (rest os))
          2 (recur (apply-operation* * p positions)
                   (first os)
                   (rest os))
          99 (nth p 0))))))

(defn solution-1 []
  (let [input-program (-> (read-input) (restore 12 2))
        output (run-program input-program)]
    output))

(defn solution-2 [desired-output]
  (let [nouns-and-verbs (combo/cartesian-product (range 0 100) (range 0 100))
        input-program (read-input)
        [desired-noun desired-verb] (->> nouns-and-verbs
                                         (filter
                                          (fn [[noun verb]]
                                            (= desired-output
                                               (run-program (restore input-program noun verb)))))
                                         first)]
    (+ desired-verb (* 100 desired-noun))))
