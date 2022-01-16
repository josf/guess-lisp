(ns guess-lisp.core
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set])
  (:import
   [java.io BufferedReader StringReader]))

(defn import-word-list
  [f]
  (with-open [rdr (clojure.java.io/reader (io/file  (io/resource f)))]
    (vec (filter #(= 5 (count %)) (line-seq rdr)))))


(defn compare-words
  [target-word guess]
  (let [target-set (set target-word)]
    (map (fn [t-letter g-letter]
           (cond (= t-letter g-letter)
                 {:correct t-letter}
                 (target-set g-letter)
                 {:in-word g-letter}
                 :else
                 nil))
         target-word guess)))

(defn win?
  [match]
  (every? :correct match))


(defn chars->pred
  [chars]
  (fn [word]
    (when (every? identity
                  (map (fn [w c]
                         (or (not c)
                             (= c w)))
                       word chars))
      word)))

(defn exact-match-predicate
  [matches]
  (if (zero? (count matches))
        (constantly false)
        (chars->pred
         (reduce
          (fn [m1 m2]
            (map (fn [l1 l2] (some :correct [l1 l2])) m1 m2))
          [nil nil nil nil nil]
          matches))))

(defn aggregate-letter-match-set
  [matches]
  (->> matches
       (mapcat (fn [m]
                 (keep (fn [m-item]
                         (or (:correct m-item)
                             (:in-word m-item))) m)))
       set))

(defn letter-in-word-predicate
  [matches]
  (if (not (seq matches))
    (constantly false)

    (let [match-set (aggregate-letter-match-set matches)]
      (fn [word]
        (let [word-set (set word)]
          (set/subset? match-set word-set))))))

