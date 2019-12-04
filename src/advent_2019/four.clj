(ns advent-2019.four)

(defn to-int [s]
  (Integer/parseInt s))

(defn password-range []
  (range 284639 748759))

(defn number->digit-pairs [n]
  (map
   #(map (comp to-int str) %)
   (partition 2 1 (str n))))

(defn two-digits-identical? [digit-pairs]
  (some (partial apply =) digit-pairs))

(defn contains-digit-double? [digit-pairs]
  (let [repeated-digits (map first (filter (partial apply =) digit-pairs))]
    (some (fn [e] (= 1 (count (val e))))
          (group-by identity repeated-digits))))

(defn non-decreasing-digits? [digit-pairs]
  (every? (partial apply <=) digit-pairs))

(defn solution-1 []
  (->> (password-range)
       (map number->digit-pairs)
       (filter two-digits-identical?)
       (filter non-decreasing-digits?)
       count))

(defn solution-2 []
  (->> (password-range)
       (map number->digit-pairs)
       (filter contains-digit-double?)
       (filter non-decreasing-digits?)
       count))
