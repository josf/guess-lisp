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

(defn remove-exact-matches-from-exclusions
  "If the guess includes the same letter twice, and only one is present
  in the target word, and one of the guesses is a perfect match, the
  other will be displayed grey, as `not-in-word`. We don't want this
  to cause a miss when comparing sets, so we remove those from the
  word-level `:excluded` set."
  [{:keys [letters] :as accumulated-matches}]
  (let [matched-letters (set (keep :match letters))]
    (update accumulated-matches :excluded set/difference matched-letters)))

(defn omni-pred-data
  [matches]
  (reduce
    (fn [acc m]
      (-> acc
          (update :excluded (fn [x] (set/union x (set (keep :not-in-word m)))))
          (update :required (fn [r] (set/union r (set (keep :in-word m)))))
          (update :letters (fn [ls]
                             (map
                               (fn [l {:keys [in-word correct]}]
                                 (cond correct
                                       (assoc l :match correct)

                                       in-word
                                       (update l :excluded conj in-word)

                                       :else
                                       l))
                               ls m)))
          remove-exact-matches-from-exclusions))
    {:excluded #{}
     :required #{}
     :letters [{:excluded #{}}
               {:excluded #{}}
               {:excluded #{}}
               {:excluded #{}}
               {:excluded #{}}]}
    matches))

(defn omni-pred
  [{:keys [excluded required letters]}]
  (fn [word]
    (let [word-letter-set (set word)]
      (cond (seq (set/intersection word-letter-set excluded))
            nil

            (seq (set/difference required word-letter-set))
            nil

            (not (every? identity (map (fn [{:keys [match excluded]} w-l]
                                         (cond (and match (= match w-l))
                                               true
                                               (and match (not= match w-l))
                                               false
                                               (excluded w-l)
                                               false
                                               :else
                                               true))
                                       letters
                                       word)))
            nil
            
            :else
            word))))

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
