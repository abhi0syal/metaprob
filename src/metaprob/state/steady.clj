(ns metaprob.state.steady
  (:require [metaprob.state.core :as core]))

(declare state-to-map)

(defn has-value? [state]
  (not (= (get (state-to-map state) :value :no-value) :no-value)))

(defn value [state]
  (get (state-to-map state) :value))

(defn has-subtrace? [state key]
  (contains? (state-to-map state) key))

(defn subtrace [state key]
  (let [val (get (state-to-map state) key)]
    (assert (not (= val nil))
            ["no such subtrace" key state])
    val))

(defn state-keys [state]
  (core/keys-sans-value (state-to-map state)))

(defn subtrace-count [state]
  (count (state-keys state)))

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

(def value-only-trace? core/value-only-trace?)
