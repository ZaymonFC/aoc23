(ns trie)

(defn add-to-trie [trie x]
  (assoc-in trie x (merge (get-in trie x) {:val x :terminal true})))

(defn in-trie? [trie x]
  "Returns true if the value x exists in the specified trie."
  (:terminal (get-in trie x) false))

(defn prefix-matches [trie prefix]
  "Returns a list of matches with the prefix specified in the trie specified."
  (keep :val (tree-seq map? vals (get-in trie prefix))))

(defn on-path? [trie prefix]
  ((complement nil?) (not-empty (prefix-matches trie prefix))))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))
