(ns clojush.examples.sturmian
  (:use [clojush.pushgp.pushgp]
        [clojush.pushstate]
        [clojush.interpreter]
        [clojure.math.numeric-tower]))

(defn gen-MESS
  "Generates an 'n' length sequence of a Minimum Ex with Skipping Set (MESS).
In this algorithm, the resultant MESS comprises every positive integer that is not an outsider.
The initial outsider is 2. Every subsequent outsider is always 2 greater than the current outsider plus the difference between the next repeater and the current repeater.
The repeater is an integer that starts at 0, repeats for a variable number of loop cycles, then increments by 1. How many times a given repeater repeats depends on whether or not it is itself a member of the MESS, and is given by the parameters 'member-repititions' and 'outsider-reptitions', respectively."
  [n member-repititions outsider-repititions]
  ;First, asserts that neither repititions parameter is negative, and that at least one is positive
  {:pre [(not (neg? member-repititions))
         (not (neg? outsider-repititions))
         (or (pos? member-repititions)
             (pos? outsider-repititions))]}
  ;The loop initializes, with (zero? outsider-reptitions) being a special case that alters the initial values
  (loop [repeater (if (zero? outsider-repititions) 1 0)
         repeater-countdown (if (zero? outsider-repititions)
                              member-repititions
                              outsider-repititions)
         outsider (if (zero? outsider-repititions) 3 2)
         MESS (if (zero? outsider-repititions) (sorted-set 1 2) (sorted-set 1))]
    ;The loop ends if we've gathered 'n' or more MESS members, otherwise it recurs
    (if (<= n (count MESS))
      (take n MESS)
      (let [[next-repeater
             next-repeater-countdown] (if (< 1 repeater-countdown)
                                        [repeater (dec repeater-countdown)]
                                        ;Once a repeater-countdown reaches 1, it is time to select a new repeater (and its corresponding repeater-countdown)
                                        (loop [new-r (inc repeater)]
                                          (let [new-r-countdown (if (contains? MESS new-r)
                                                                  member-repititions
                                                                  outsider-repititions)]
                                            ;The loop increments up from the current until it finds a new repeater whose corresponding countdown is not zero
                                            (if-not (zero? new-r-countdown)
                                              [new-r new-r-countdown]
                                              (recur (inc new-r))))))
            next-outsider (+ outsider 2 (- next-repeater repeater))
            next-MESS (into MESS (range (inc outsider) next-outsider))] ;Integers between two outsiders get included in the MESS
        (recur next-repeater
               next-repeater-countdown
               next-outsider
               next-MESS)))))

(def example-MESS (gen-MESS 50 1 0))

(defn protected-division
  [x y]
  (if (zero? y)
    x
    (/ x y)))

(defn abs-diff
  [x y]
  (Math/abs (- x y)))

(defn continued-fraction
  ([] 1)
  ([x] x)
  ([x y & remaining]
    (->> (apply continued-fraction remaining)
         (protected-division y)
         (+ x))))

(defn to-output
  [n integers]
  (->> integers
       (apply continued-fraction)
       (* (inc n))
       int))

(def argmap
  {:error-function (fn [program]
                     (if-let [integers (->> (make-push-state)
                                            (run-push program)
                                            :integer)]
                       (doall
                         (for [input (range 50)]
                           (abs-diff (to-output input integers)
                                     (nth example-MESS input))))
                     1000))
   :atom-generators (list (fn [] (- (rand-int 21) 10)))})

(for [input (range 50)]
  (abs-diff (to-output input [1 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1])
            (nth example-MESS input)))
