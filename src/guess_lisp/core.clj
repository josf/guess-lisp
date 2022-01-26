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
  "Builds up the filtering data necessary for `omni-pred`. See that
  function for an explanation."
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
  "A single predicate that applies all the information gathered by
  `omni-pred-data` and filters the list of words.

  There are two strategies here:

  On the whole-word level, we use eliminated letters (gray in Wordle)
  and in-word (but in wrong position) letters to exclude words that
  either contain or don't contain the letters in those two sets.

  At the letter-position level, we either detect a perfect match, or
  accumulate exclusions. These letter-level exclusions correpond to
  the in-word letters, so this is the inference that the letter in
  question is in the word, but not in *this* position."
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

(defn letter-frequencies-by-position
  "For a given letter position, returns a map where the keys are the
  letters of the alphabet and the values are their frequencies in the
  list of words provided."
  [words position]
  (frequencies (map #(nth % position) words)))

(defn- deduped-word-score
  "Words that have repeated letters that occur frequently end up with
  higher scores, while they don't provide data that is as good as
  words with only singleton letters."
  [word freqs]
  (apply +
         (vals
           (reduce
             (fn [acc [letter fqs]]
               (assoc acc letter (get fqs letter)))
             {}
             (map vector word freqs)))))

(defn ranked-guesses
  "Based on a list of words, scores each word by the frequency of each
  of its letters at their respective positions. (If there are more 's'
  at position 2 than at position 3, the letter will have a higher
  score if it's in position 2.)

  This appears to be a decent strategy if the guesses are recalculated
  after every failed guess, on the remaing words.

  The strategy here is to always select the most probable match. This
  has the best chances of matching, and on failure eliminates the most
  possible words, providing better information for the next guess."
  [words]
  (let [freqs (map (partial letter-frequencies-by-position words) (range 5))]
    (->> words
         (map
           (fn [word]
             {:word word
              :score 
              (deduped-word-score word freqs)}))
         (sort-by :score))))


(defn simulate
  "Runs a simulated game."
  [target-word words matches guess-count]
  (let [filtered-words (keep (omni-pred (omni-pred-data matches)) words)
        ranked (ranked-guesses filtered-words)]
    (cond (> guess-count 6)
          {:failed-after 6 :word target-word :matches matches}

          (= 1 (count ranked))
          {:found (:word (first ranked)) :guesses guess-count}

          :else
          (recur target-word filtered-words
                 (conj matches (compare-words target-word (:word (last ranked)))) (inc guess-count)))))


(defn word-response
  "Convenience function for manually entering a real Wordle from the website.
  x = correct letter (`:correct`),
  ! = wrong letter (`:not-in-word`),
  ? = correct letter but wrong position. (`:in-word`)"
  [word wordle-response]
  (when (not= 5 (count wordle-response))
    (throw (ex-info "Incorrect wordle reponse length"  {:length (count wordle-response)})))
  (map (fn [letter code]
         (case code
           \x {:correct letter}
           \! {:not-in-word letter}
           \? {:in-word letter}))
       word wordle-response))


(comment
  (ranked-guesses (keep (omni-pred
                         (omni-pred-data
                          [[{:not-in-word \s}
                            {:in-word \o}
                            {:not-in-word \r}
                            {:not-in-word \e}
                            {:not-in-word \s}]
                           [{:not-in-word \a}
                            {:in-word \l}
                            {:correct \o}
                            {:not-in-word \o}
                            {:not-in-word \f}]
                           [{:not-in-word \g}
                            {:not-in-word \h}
                            {:correct \o}
                            {:not-in-word \u}
                            {:correct \l}]])) z))

  (ranked-guesses (keep (omni-pred
                         (omni-pred-data
                          [[{:correct \s}
                            {:not-in-word \o}
                            {:in-word \r}
                            {:not-in-word \e}
                            {:not-in-word \s}]
                           [{:correct \s}
                            {:not-in-word \t}
                            {:in-word \a}
                            {:in-word \r}
                            {:not-in-word \s}]]))
                        z))

  (ranked-guesses (keep (omni-pred
                          (omni-pred-data
                            [(word-response "cares" "??!!!")
                             (word-response "alack" "!!xxx")]))
                        z)))
