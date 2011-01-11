(ns one.core
  (:require [clojure.zip :as zip])
  (:gen-class))

(def naturals (iterate #(+ 1 %1) 0))
(def n 3)
(def N (* n n))
(def indexes (range N))
(def nums (range 1 (+ 1 N)))
(def puzzle [[0 7 6 0 0 0 0 4 0]
             [0 9 0 0 5 7 6 0 8]
             [0 4 0 9 0 0 3 0 0]
             [0 6 0 0 0 8 4 0 0]
             [9 0 2 5 0 4 7 0 3]
             [0 0 3 6 0 0 0 8 0]
             [0 0 9 0 0 6 0 3 0]
             [7 0 5 1 4 0 0 9 0]
             [0 3 0 0 0 0 2 7 0]])

(defn -main [& args]
  (println "Hello, world!"))

(defn any [lst] (some identity lst))

(defn coord [grid r c] (nth (nth grid r) c))

(defn nth-square [i grid]
  (let [r (* n (int (/ i 3)))
        c (* n (mod i 3))]
    (for [x (range 3) y (range 3)]
      (coord grid (+ r x) (+ c y)))))

(defn rows [grid] grid)
(defn cols [grid] (map (fn [i] (map #(nth %1 i) grid)) indexes))
(defn squares [grid] (map #(nth-square %1 grid) indexes))

(defn no-doubles? [lst] (apply distinct? (filter pos? lst)))
(defn valid? [grid]
  (and
    (every? no-doubles? (rows grid))
    (every? no-doubles? (cols grid))
    (every? no-doubles? (squares grid))
    grid))

(defn fill [loc]
  (let [options (map #(zip/next (zip/replace loc %1)) nums)]
    (filter #(valid? (zip/root %1)) options)))
(defn search [loc]
  (cond
    (zip/end? loc) (zip/root loc)
    (vector? (zip/node loc)) (search (zip/next loc))
    (pos? (zip/node loc)) (search (zip/next loc))
    :else (any (map search (fill loc)))))
(defn solve [grid] (search (zip/vector-zip grid)))

(println (solve puzzle))
