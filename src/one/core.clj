(ns one.core
  (:require [clojure.zip :as zip])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.contrib.duck-streams :as io])
  (:require [clojure.contrib.string :as string])
  (:use clojure.set)
  (:gen-class))

; Utility functions
(defn any [lst] (first (filter identity lst)))
(defn snap [x n] (- x (mod x n)))
(defn vecmap [f a] (vec (map f a)))

(defn input [file]
  (let [raw (io/read-lines file)
        lists (map #(string/split #"\s" %) raw)
        convert #(if (= % "-") 0 (Integer/parseInt %))]
    (vecmap #(vecmap convert %) lists)))

(defn solve [grid]

  ; Define grid-dependant constants
  (let [
    counts (map count grid)         ; The size of each row
    length (apply + counts)         ; Number of elements in grid
    side (math/sqrt length)         ; The size of one side of the whole square
    n (math/sqrt side)              ; The size of one side of an inner square
    nums (set (range 1 (+ 1 side)))]

  ; Define grid-dependant functions
  (letfn [
    ; Grid traversal
    (nth-col [grid i] (take-nth side (drop i (apply concat grid))))
    (coord-square [grid r c]
      (let [i (snap r n), j (snap c n)]
        (for [x (range n) y (range n)]
          (nth (nth grid (+ i x)) (+ y j)))))
    
    ; Zipper manipulation
    (row-of [loc] (zip/node (zip/up loc)))
    (col-of [loc] (nth-col (zip/root loc) (count (zip/lefts loc))))
    (sqr-of [loc] (coord-square (zip/root loc)
      (count (zip/lefts (zip/up loc)))
      (count (zip/lefts loc))))

    ; Search functions
    (options [loc]
      (difference nums (set (union (row-of loc) (col-of loc) (sqr-of loc)))))
    (fill [loc] (map #(zip/next (zip/replace loc %)) (options loc)))
    (search [loc]
      (cond
        (zip/end? loc) (zip/root loc)
        (vector? (zip/node loc)) (search (zip/next loc))
        (pos? (zip/node loc)) (search (zip/next loc))
        :else (any (map search (fill loc)))))]

  ; Ensure that the grid is well-formed
  (assert (integer? n))           ; The input is a valid length
  (assert (apply = counts))       ; Each row is the same length
  (assert (= side (count grid)))  ; The input is square

  ; Solve the grid
  (search (zip/vector-zip grid)))))

(defn -main [file] (dorun (map println (solve (input file)))))
