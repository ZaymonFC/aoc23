(ns aoc23.day-3 #_{:clj-kondo/ignore [:refer-all :unused-namespace]}
    (:require [prelude :as p]
              [clojure.string :as string]))

;; --- Data --------------------------------------------------------------------
(def sample (->> (slurp "inputs/03-sample.txt") p/lines))
(def input (->> (slurp "inputs/03.txt") p/lines))

;; --- Common ------------------------------------------------------------------
(defn neighbors [x y]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (not (and (= dx 0) (= dy 0)))]
    [(+ x dx) (+ y dy)]))

;; --- Part 1 ------------------------------------------------------------------
(defn neighbours-for-x-span [x-start x-end y]
  {:pre [(<= x-start x-end)]}
  (->> (for [x (range x-start x-end)] (neighbors x y))
       (apply concat)
       (into #{})))

(defn get-xy [lines x y]
  (try
    (-> lines (nth y) (nth x))
    (catch IndexOutOfBoundsException _exception nil)))

(defn find-pattern-in-schematic [pattern schematic]
  (->> schematic
       (map-indexed (fn [y line] [y (p/re-seq-pos pattern line)]))
       (filter (fn [[_y line]] (not (nil? line))))
       (map (fn [[y line]] (map (fn [x] (assoc x :y y)) line)))
       (apply concat)))

(defn part-symbol? [x]
  (and (not (p/char-is-digit? x)) (not= \. x)))

(defn valid-part? [lines x-start x-end y]
  (->> (neighbours-for-x-span x-start x-end y)
       (some
        (fn [[x y]]
          (let [val (get-xy lines x y)]
            (if (nil? val) nil (part-symbol? (get-xy lines x y))))))))

;; Answer
(->> input
     (find-pattern-in-schematic #"\d+")
     (filter (fn [{:keys [start end y]}] (valid-part? input start end y)))
     (map :group)
     (map p/parse-int)
     (apply +))

;; --- Part 2 ------------------------------------------------------------------
(defn point-in-x-span? [point-x point-y x-start x-end y]
  (and (<= x-start point-x (dec x-end))
       (= y point-y)))

(defn point-adjacent-part-numbers [[point-x point-y] part-numbers]
  (let [neighbors (neighbors point-x point-y)]
    (->> part-numbers
         (filter (fn [{:keys [start end y]}]
                   (some (fn [[neighbour-x neighbour-y]]
                           (point-in-x-span? neighbour-x neighbour-y start end y))
                         neighbors))))))

;; Answer
(let [part-numbers (find-pattern-in-schematic #"\d+" input)
      gears (->> (find-pattern-in-schematic #"\*" input)
                 (map (fn [{:keys [start y]}] [start y])))]

  (->> gears
       (map (fn [gear] (point-adjacent-part-numbers gear part-numbers)))
       (filter (fn [x] (= (count x) 2)))
       (map (partial map :group))
       (map (partial map p/parse-int))
       (map (partial apply *))
       (apply +)))
