(ns metaprob.state.cond
  (:require [metaprob.state.core :as core]))

(declare state-to-map)

(defn has-value? [state]
    (cond (seq? state) (not (empty? state))
          (vector? state) false
          (map? state) (not (= (get state :value :no-value) :no-value))
        true (assert false ["not a state" state])))

(defn value [state]
    (cond (seq? state) (first state)
          (vector? state) (assert false "no value")
          (map? state)
          (let [value (get state :value :no-value)]
            (assert (not (= value :no-value)) ["state has no value" state])
            value)
        true (assert false ["not a state" state])))

(defn has-subtrace? [state key]
    (cond (seq? state) (and (not (empty? state))
                          (= key core/rest-marker))
          (vector? state) (and (integer? key)
                               (>= key 0)
                               (< key (count state)))
          (map? state) (contains? state key)
        true (assert false ["not a state" state])))

(defn subtrace [state key]
  (let [val (cond (seq? state) (rest state)
                    (vector? state) {:value (nth state key)}
                    (map? state) (get state key)
                  true (assert false ["not a state" state]))]
    (assert (not (= val nil))
            ["no such subtrace" key state])
    val))

(defn state-keys [state]
  (cond (seq? state) (if (empty? state) '() (list core/rest-marker))
          (vector? state) (range (count state))
        (map? state) (core/keys-sans-value state)
        true (assert false ["not a state" state])))

(defn subtrace-count [state]
    (cond (seq? state) (if (empty? state) 0 1)
          (vector? state) (count state)
          (map? state) (let [n (count state)]
                         (if (= (get state :value :no-value) :no-value)
                           n
                           (- n 1)))
        true (assert false ["not a state" state])))

(defn state-to-map [state]
  (cond (map? state) state

        (seq? state)
        (if (empty? state)
          {}
          {:value (first state) core/rest-marker (rest state)})

        (vector? state)
        (into {} (map (fn [i x] [i {:value x}])
                      (range (count state))
                      state))

        true (assert false ["not a state" state])))
