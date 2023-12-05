(ns aoc23.day-2 (:require [prelude :refer :all]
                          [clojure.string :as string]))


;; --- Parsing -----------------------------------------------------------------
(defn game-number [s] (let [regex #"\bGame (\d+)"] (parse-int (second (re-find regex s)))))

(def pair-regex #"\b(\d+)\s+(\w+)\b")

(defn parse-turn [s]
  (->> s
       (re-seq pair-regex)
       (map (fn [[_ n color]] [(keyword color) (parse-int n)]))))

(defn parse-game [s]
  (let [game (game-number s)
        turns-part (-> s (string/split #":") second (string/split #";"))]
    {:game game
     :turns (map parse-turn turns-part)}))

;; --- Putting it all together -------------------------------------------------
(def input (->> (slurp "inputs/02-1.txt") lines))

;; Part 1
(def valid-bag {:red 12 :green 13 :blue 14})

(defn valid-turn? [turn]
  (every?
   (fn [[color n]] (<= n (get valid-bag color 0)))
   turn))

(->> input
     (map parse-game)
     (filter (fn [{:keys [turns]}] (every? valid-turn? turns)))
     (map :game)
     (apply +))

;; Part 2
(defn min-viable-bag [turns] (reduce (partial merge-with max) turns))

(defn bag-power [bag] (reduce * (vals bag)))

(->> input
     (map parse-game)
     (map :turns)
     (map (partial map (partial into {})))
     (map min-viable-bag)
     (map bag-power)
     (apply +))

