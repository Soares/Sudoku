(ns one.core
  (:require [clojure.zip :as zip])
  (:require [clojure.contrib.math :as math])
  (:require [clojure.contrib.duck-streams :as io])
  (:require [clojure.contrib.string :as string])
  (:use clojure.set)
  (:gen-class))

(defn input [file]
  (let [raw (io/read-lines file)
        zeroed (map #(string/replace-char \- \0 %) raw)
        words (map #(string/split #"\s" %) zeroed)
        data (map (partial map #(Integer/parseInt %)) words)]
    (vec (map vec data))))

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
      (let [i (- r (mod r n))
            j (- c (mod c n))]
        (for [x (range n) y (range n)]
          (nth (nth grid (+ i x)) (+ y j)))))
    
    ; Zipper manipulation
    (row-of [loc] (zip/node (zip/up loc)))
    (col-of [loc] (nth-col (zip/root loc) (count (zip/lefts loc))))
    (sqr-of [loc] (coord-square (zip/root loc)
      (count (zip/lefts (zip/up loc)))
      (count (zip/lefts loc))))

    ; Zipper searching
    (taken [loc] (set (union (row-of loc) (col-of loc) (sqr-of loc))))
    (options [loc] (difference nums (taken loc)))
    (fill [loc] (map #(zip/next (zip/replace loc %)) (options loc)))
    (search [loc]
      (cond
        (zip/end? loc) (zip/root loc)
        (vector? (zip/node loc)) (search (zip/next loc))
        (pos? (zip/node loc)) (search (zip/next loc))
        :else (some identity (map search (fill loc)))))]

  ; Ensure that the grid is well-formed
  (assert (integer? n))           ; The input is a valid length
  (assert (apply = counts))       ; Each row is the same length
  (assert (= side (count grid)))  ; The input is square

  (search (zip/vector-zip grid)))))

(defn -main [file] (dorun (map println (solve (input file)))))
