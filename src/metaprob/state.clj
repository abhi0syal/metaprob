;; Trace states

(ns metaprob.state
  (:require [potemkin :as potemkin]
            [metaprob.state.cond]
            [metaprob.state.core :as core]
            [metaprob.state.protocol]))

(declare map-to-state)

;; Basic trace operations
(potemkin/import-vars [metaprob.state.core
                       map-to-state
                       rest-marker])

(potemkin/import-vars [metaprob.state.protocol
                       has-value?
                       value
                       has-subtrace?
                       subtrace
                       state-keys
                       subtrace-count
                       state-to-map
                       value-only-trace?])

(defn state?
  [x]
  ;; satisfies? is slow https://dev.clojure.org/jira/browse/CLJ-1814
  (or (seq? x)
      (vector? x)
      (map? x)))

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
