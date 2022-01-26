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


(defn letter-frequencies-by-position
  [words position]
  (frequencies (map #(nth % position) words)))

(defn- deduped-word-score
  [word freqs]
  (apply +
         (vals
           (reduce
             (fn [acc [letter fqs]]
               (assoc acc letter (get fqs letter)))
             {}
             (map vector word freqs)))))

(defn ranked-guesses
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

  (best-guess (keep (omni-pred
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


(best-guess (keep (omni-pred
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


(best-guess (keep (omni-pred
                    (omni-pred-data
                      [(word-response "cares" "??!!!")
                       (word-response "alack" "!!xxx")]))
                  z)))
