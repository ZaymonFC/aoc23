(ns aoc23.day-4 #_{:clj-kondo/ignore [:refer-all :unused-namespace]}
    (:require [prelude :as p]
              [clojure.string :as string]
              [clojure.set :as set]))

;; --- Data --------------------------------------------------------------------
(def sample (->> (slurp "inputs/04-sample.txt") p/lines))
(def input (->> (slurp "inputs/04.txt") p/lines))

;; --- Common ------------------------------------------------------------------
(defn parse-card [line]
  (let [card-number (re-find #"\d+" line)
        numbers-part (second (string/split line #":"))
        [winning-numbers-part your-numbers-part] (string/split numbers-part #"\|")]

    {:card card-number
     :winning (p/parse-numbers-string winning-numbers-part)
     :yours (p/parse-numbers-string your-numbers-part)}))

;; --- Part 1 ------------------------------------------------------------------
(defn matching-numbers
  "Count the number of winning numbers in the card"
  [{:keys [winning yours]}]
  (count (set/intersection (into #{} winning) (into #{} yours))))

(defn score-card
  "A card is worth 2^(n-1) where n is the number of matching numbers"
  [card]
  (let [n  (matching-numbers card)]
    (if (= n 0) 0 (p/exp 2 (dec n)))))

(->> input
     (map parse-card)
     (map score-card)
     (apply +))

;; --- Part 2 ------------------------------------------------------------------
(def card-scores
  (->> input
       (map parse-card)
       (map matching-numbers)
       (map-indexed (fn [idx score] [idx score]))
       (into {})))

(defn iterate-n-times [n f x]
  (nth (iterate f x) n))

(->>
 (loop [card 0
        copies (->> (keys card-scores) (map (fn [k] [k 1])) (into {}))]
   (if (= card (count card-scores))
     copies
     (let [score (get card-scores card)
           times-to-apply (get copies card)]
       (recur
        (inc card)
        (iterate-n-times
         times-to-apply
         (fn [c] (p/update-vals c (range (inc card) (+ card score 1)) inc))
         copies)))))
 vals
 (apply +))



