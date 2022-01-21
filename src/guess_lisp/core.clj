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


(defn exclusions-per-position
  [matches]
  (reduce
    (fn [m1 m2]
      (map (fn [pos-m1 pos-m2]
             (if-let [in-word-letter (:in-word pos-m2)]
               (conj pos-m1 in-word-letter)
               pos-m1))
           m1 m2))
    [#{} #{} #{} #{} #{}]
    matches))

(defn exclude-in-word-perfect-matches-predicate
  [matches]
  (let [exclusions (exclusions-per-position matches)]
    (fn [word]
      (when (every? identity
                  (map
                    ;; true = ok, letters don't match, means this pair does not exclude anything
                    ;; false = oops, match, so this pair means the word doesn't match
                   (fn [letter excl-set] (not (excl-set letter)))
                   word
                   exclusions))
        word))))

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
(comment
  (non-matching-letters-set  [(compare-words "solar" "power") (compare-words "solar" "eeeee")]))

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

(comment
  (keep (exact-match-predicate [(compare-words "solar" "power")]) z))



(defn multi-predicate
  [matches]
  (let [perfect-match? (exact-match-predicate matches)
        in-words-are-not-perfect-match? (exclude-in-word-perfect-matches-predicate matches)
        letter-match? (letter-in-word-predicate matches)
        letters-not-excluded? (letters-not-excluded-predicate matches)]
    (fn [word]
      (and
        (perfect-match? word)
           (in-words-are-not-perfect-match? word)
           (letter-match? word)
           (letters-not-excluded? word)))))

(comment
  (keep
   (multi-predicate [[{:correct \s} {:not-in-word \t} {:in-word \e} {:not-in-word \a} {:not-in-word \m}]
                     [{:in-word \r} {:not-in-word \o} {:not-in-word \u} {:not-in-word \n} {:not-in-word \d}]
                     [{:not-in-word \l} {:in-word \i} {:not-in-word \v} {:in-word \e} {:in-word \r}]
                     [{:correct \s} {:in-word \e} {:in-word \r} {:in-word \i} {:not-in-word \f}]]) z)
  ;; =>   ("shire" "spire")


   (keep
     (multi-predicate [[{:not-in-word \s} {:in-word \t} {:not-in-word \e} {:not-in-word \a} {:not-in-word \m}]
                       [{:correct \r} {:correct \o} {:not-in-word \u} {:not-in-word \n} {:not-in-word \d}]])
    z)
   ;; => ("robot" "rotor")
   )
