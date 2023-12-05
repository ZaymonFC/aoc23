(ns prelude
  (:require [clojure.string :as string]))

;; --- Basic Helpers ----------------------------------------------------------
(defn lines [s] (string/split-lines s))

(defn char-is-digit? [ch] (Character/isDigit ch))

(defn first-and-last [xs]
  (let [n (count xs)]
    (cond
      (= n 1) (flatten [xs xs])
      (= n 2) xs
      (> n 2) [(first xs) (last xs)])))

(defn parse-int [s] (Integer/parseInt s))
