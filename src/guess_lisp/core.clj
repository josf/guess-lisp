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
                 {:not-in-word g-letter}))
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

(defn letter-match-set
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

    (let [match-set (letter-match-set matches)]
      (fn [word]
        (let [word-set (set word)]
          (when (set/subset? match-set word-set)
            word))))))
(comment
  ((letter-in-word-predicate [(compare-words "solar" "power")
                              (compare-words "solar" "eeeee")])
   "aaaaa")
  ((letter-in-word-predicate [(compare-words "solar" "power")
                              (compare-words "solar" "eeeee")])
   "orser"))


(defn non-matching-letters-set
  [matches]
  (->> matches
       (mapcat (fn [m] (keep :not-in-word m)))
       set))
(non-matching-letters-set  [(compare-words "solar" "power") (compare-words "solar" "eeeee")])

(defn letters-not-excluded-predicate
  [matches]
  (if (not (seq matches))
    (constantly false)

    (let [not-in-word-set (non-matching-letters-set matches)]
      (fn [word]
        (let [word-set (set word)]
          (when (empty? (set/intersection not-in-word-set word-set))
            word))))))

(comment
  ((letters-not-excluded-predicate [(compare-words "solar" "power")
                                    (compare-words "solar" "eeeee")])
   "ooooo") ;; => true, because o is not excluded
  ((letters-not-excluded-predicate [(compare-words "solar" "power")
                                    (compare-words "solar" "eeeee")])
   "eeeee")) ;; => false, because e is excluded

(keep (exact-match-predicate [(compare-words "solar" "power")]) z)

(defn triple-predicate
  [matches]
  (let [perfect-match? (exact-match-predicate matches)
        letter-match? (letter-in-word-predicate matches)
        letters-not-excluded? (letters-not-excluded-predicate matches)]
    (fn [word]
      (and (perfect-match? word)
           (letter-match? word)
           (letters-not-excluded? word)))))
