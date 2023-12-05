(ns aoc23.day-1 (:require [trie :refer [build-trie in-trie? on-path?]]
                          [prelude :refer :all]))


;; --- Data Structures --------------------------------------------------------
(def word-to-number
  {"zero" \0
   "one" \1
   "two" \2
   "three" \3
   "four" \4
   "five" \5
   "six" \6
   "seven" \7
   "eight" \8
   "nine" \9})

(def number-trie (build-trie (keys word-to-number)))

;; --- Logic ------------------------------------------------------------------
(defn scanner [s] {:pre [(seq? s)]}
  (let [scan
        (reduce
         (fn [acc element]
           (let [acc (conj acc element)]
             (cond
               (in-trie? number-trie acc) (reduced acc)
               (on-path? number-trie acc) acc
               :else (reduced nil))))
         [] s)]

    (if (in-trie? number-trie scan)
      (->> scan (apply str) word-to-number)
      nil)))

(defn find-numbers [s]
  (loop [s (seq s) acc []]
    (let [first-char (first s)]
      (cond
        (empty? s) acc
        (char-is-digit? first-char) (recur (rest s) (conj acc first-char))
        :else
        (if-let [scan (scanner s)]
          (recur (rest s) (conj acc scan))
          (recur (rest s) acc))))))

;; --- Putting it all together -------------------------------------------------
(def input (->> (slurp "inputs/01-1.txt") lines))

;; part 1
(->> input
     (map seq)
     (map (partial filter char-is-digit?))
     (map first-and-last)
     (map (partial apply str))
     (map (fn [n] (Integer/parseInt n)))
     (apply +))

;; part 2
(->> input
     (map find-numbers)
     (map first-and-last)
     (map (partial apply str))
     (map (fn [n] (Integer/parseInt n)))
     (apply +))

