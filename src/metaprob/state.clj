;; Trace states

(ns metaprob.state
  (:require [potemkin :as potemkin]
            [metaprob.state.cond]
            [metaprob.state.protocol]))

(declare map-to-state state-to-map keys-sans-value)

;; The `steady?` flag controls a choice between a simple but slower
;; implementation and an obscure but faster implementation of the
;; primitives.

(def steady? false)

(def rest-marker "rest")

;; Basic trace operations

(potemkin/import-vars [metaprob.state.protocol
                       state?
                       has-value?
                       value
                       has-subtrace?
                       subtrace
                       state-keys
                       subtrace-count
                       state-to-map])

; Constructors

(defn empty-state []
  '())

(defn set-value [state val]
  (map-to-state (assoc (state-to-map state) :value val)))

(defn clear-value [state]
  (map-to-state (dissoc (state-to-map state) :value)))

(defn set-subtrace [state key sub]
  ;; sub is a trace but not necessarily a sub
                                        ;(if (= sub '())
                                        ;  state
  (map-to-state (assoc (state-to-map state) key sub)))

(defn clear-subtrace [state key]
  (map-to-state (dissoc (state-to-map state) key)))

(defn value-only-trace? [tr]
  (and (map? tr)
       (= (count tr) 1)
       (contains? tr :value)))

;; Convert hash-map to heterogeneous canonical clojure form

(defn map-to-state [m]
  (let [n (count m)]
    (cond (and (= n 2)
               (not (= (get m :value :no-value) :no-value))
               (seq? (get m rest-marker :no-value)))
          (cons (get m :value)
                (get m rest-marker))

          (= n 0) '()                   ;Kludge to ensure seq-ness

          (every? (fn [n] (value-only-trace? (get m n :no-value))) (range n))
          (vec (for [i (range n)] (get (get m i) :value)))

          true (do (assert (map? m) ["expected a map" m])
                   (doseq [entry m] true)    ;Don't be lazy!
                   m))))
