(ns advent-2019.three
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn read-input []
  (str/split (slurp "resources/inputs/three.txt") #"(,|\s)"))

(defn parse-move [unparsed-move]
  (let [direction (first unparsed-move)
        unparsed-distance (apply str (rest unparsed-move))
        distance (Integer/parseInt unparsed-distance)]
    [direction distance]))

(defn parse-wire [unparsed-wire]
  (mapv
   parse-move
   (str/split unparsed-wire #",")))

(defn read-wires []
  (let [unparsed-wires (str/split (slurp "resources/inputs/three.txt") #"\n")]
    (mapv parse-wire unparsed-wires)))

(defn traverse-wire [wire]
  (partition 2 1
             (reduce
              (fn [endpoints [direction distance]]
                (let [[current-x current-y] (last endpoints)]
                  (case direction
                    \U (conj endpoints [current-x (+ current-y distance)])
                    \D (conj endpoints [current-x (- current-y distance)])
                    \L (conj endpoints [(- current-x distance) current-y])
                    \R (conj endpoints [(+ current-x distance) current-y]))))
              [[0, 0]]
              wire)))

(defn vertical? [[[x1 _] [x2 _]]]
  (= x1 x2))

(defn horizontal? [[[_ y1] [_ y2]]]
  (= y1 y2))

(defn perpendicular? [segment1 segment2]
  (or
   (and (horizontal? segment1) (vertical? segment2))
   (and (horizontal? segment2) (vertical? segment1))))

(defn non-central? [[[x y] _]]
  (not= x y 0))

(defn intersects? [& segments]
  (let [horizontal (first (map sort (filter horizontal? segments)))
        vertical (first (map sort (filter vertical? segments)))
        [min-x constant-y] (first horizontal)
        [constant-x min-y] (first vertical)
        [max-x _] (last horizontal)
        [_ max-y] (last vertical)]
    (and (<= min-x constant-x max-x)
         (<= min-y constant-y max-y))))

(defn intersection-coordinate [& segments]
  (let [sorted-segments (map sort segments)
        [_ y] (ffirst (filter horizontal? sorted-segments))
        [x _] (ffirst (filter vertical? sorted-segments))]
    [x y]))

(defn crossing? [[segment1 segment2]]
  (and (perpendicular? segment1 segment2)
       (intersects? segment1 segment2)
       (non-central? segment1)))

(defn absolute-coordinates [& coords]
  (map #(Math/abs %) coords))

(defn intersections [wires]
  (->> (apply combo/cartesian-product (map traverse-wire wires))
       (filter crossing?)
       (map (partial apply intersection-coordinate))))

(defn solution-1 []
  (->> (read-wires)
       intersections
       (map (partial apply absolute-coordinates))
       (map (partial apply +))
       sort
       first))

(defn segment-length [[[x1 y1] [x2 y2]]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))))

(defn wire-length [segments]
  (apply + (map segment-length segments)))

(defn path-to [coordinate wire]
  (let [full-path (traverse-wire wire)
        terminal-segment [coordinate coordinate]
        covering-path (take-while #(not (intersects? % terminal-segment)) full-path)
        last-origin (first (last covering-path))
        last-segment (list last-origin coordinate)]
    (conj (vec (drop-last covering-path)) last-segment)))

(defn solution-2 []
  (let [wires (read-wires)
        wire-intersections (intersections wires)]
    (apply min (map (fn [intersection]
                      (apply + (map (fn [wire]
                                      (wire-length (path-to intersection wire)))
                                    wires)))
                    wire-intersections))))
