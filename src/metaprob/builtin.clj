(ns metaprob.builtin
  "This module is intended for import by metaprob code.
  To be used in conjunction with the syntax module."
  (:refer-clojure :exclude
                  [newline not + = boolean? > >= < <= - * / number?
                   and or case cond assert print apply list list?
                   first rest last nth range sort])
  (:require [metaprob.trace :as trace]
            [metaprob.sequence :as sequence]
            [metaprob.builtin-impl :as impl]))

(defmacro define-foreign-procedure [mp-name generate-fn]
  (let [namestring (if (symbol? mp-name) (str mp-name) mp-name)]
    `(do (declare ~mp-name)
         (def ~mp-name
           (impl/make-foreign-procedure ~namestring
                                        ~generate-fn)))))

;; ----------------------------------------------------------------------
;; Builtins (defined in python in original-metaprob)

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms.

;; General
(define-foreign-procedure =                  clojure.core/=)
(define-foreign-procedure neq                impl/neq)
(define-foreign-procedure assert             impl/metaprob-assert)
(define-foreign-procedure error              impl/error)
(define-foreign-procedure generate-foreign   impl/generate-foreign)
(define-foreign-procedure procedure-name     impl/procedure-name)
(define-foreign-procedure trace-name         impl/trace-name)
(define-foreign-procedure trace-as-procedure trace/trace-as-procedure)

;; Logical
(define-foreign-procedure not      clojure.core/not)
(define-foreign-procedure boolean? clojure.core/boolean?)

;; Numeric
(define-foreign-procedure >       clojure.core/>)
(define-foreign-procedure >=      clojure.core/>=)
(define-foreign-procedure <=      clojure.core/<=)
(define-foreign-procedure <       clojure.core/<)
(define-foreign-procedure add     impl/add)
(define-foreign-procedure +       clojure.core/+)
(define-foreign-procedure -       clojure.core/-)
(define-foreign-procedure *       clojure.core/*)
(define-foreign-procedure /       clojure.core//)
(define-foreign-procedure log     impl/log)
(define-foreign-procedure cos     impl/cos)
(define-foreign-procedure sin     impl/sin)
(define-foreign-procedure log1p   impl/log1p)
(define-foreign-procedure exp     impl/exp)
(define-foreign-procedure sqrt    impl/sqrt)
(define-foreign-procedure normal  impl/normal)
(define-foreign-procedure floor   impl/floor)
(define-foreign-procedure round   impl/round)
(define-foreign-procedure number? clojure.core/number?)
(define-foreign-procedure expt    impl/expt)

;; Sample from uniform distribution, with RNG as hidden state
(define-foreign-procedure sample-uniform impl/sample-uniform)

;; Traces
(define-foreign-procedure trace            trace/trace)           ; constructor
(define-foreign-procedure mutable-trace    trace/mutable-trace)   ; constructor
(define-foreign-procedure immutable-trace  trace/immutable-trace) ; constructor
(define-foreign-procedure ok-key?          trace/ok-key?)
(define-foreign-procedure empty-trace      trace/empty-trace)
(define-foreign-procedure empty-trace?     trace/empty-trace?)
(define-foreign-procedure trace-has?       trace/trace-has?)
(define-foreign-procedure trace-get        trace/trace-get)
(define-foreign-procedure trace-subtrace   trace/trace-subtrace)
(define-foreign-procedure trace-keys       trace/trace-keys)
(define-foreign-procedure trace-count      trace/trace-count)
(define-foreign-procedure trace?           trace/trace?)
(define-foreign-procedure trace-has-subtrace? trace/trace-has-subtrace?)
(define-foreign-procedure trace-subtrace   trace/trace-subtrace)
(define-foreign-procedure trace-copy       trace/trace-copy)
(define-foreign-procedure to-mutable       trace/to-mutable)
(define-foreign-procedure to-immutable     trace/to-immutable)
(define-foreign-procedure mutable-trace?   trace/mutable-trace?)
(define-foreign-procedure immutable-trace? trace/immutable-trace?)
(define-foreign-procedure **               trace/**)  ; for (trace ... (** ...) ...)

(define-foreign-procedure trace-set!          trace/trace-set!)
(define-foreign-procedure trace-delete!       trace/trace-delete!)
(define-foreign-procedure trace-set-subtrace! trace/trace-set-subtrace!)
(define-foreign-procedure trace-merge!        trace/trace-merge!)
(define-foreign-procedure trace-thaw!         trace/trace-thaw!)

(define-foreign-procedure trace-set           trace/trace-set)
(define-foreign-procedure trace-delete        trace/trace-delete)
(define-foreign-procedure trace-set-subtrace  trace/trace-set-subtrace)
(define-foreign-procedure trace-merge         trace/trace-merge)
(define-foreign-procedure trace-copy          trace/trace-copy)

(define-foreign-procedure addresses-of        impl/addresses-of)
(define-foreign-procedure addr                impl/addr)

;; Lists
(define-foreign-procedure pair  sequence/pair)
(define-foreign-procedure pair? sequence/metaprob-pair?)
(define-foreign-procedure first sequence/metaprob-first)
(define-foreign-procedure rest  sequence/metaprob-rest)
(define-foreign-procedure list  sequence/metaprob-list)
(define-foreign-procedure list? sequence/metaprob-list?)
(define-foreign-procedure last  sequence/metaprob-last)

;; Tuples
(define-foreign-procedure tuple  sequence/tuple)
(define-foreign-procedure tuple? sequence/tuple?)

;; Sequences
(define-foreign-procedure length            sequence/length)
(define-foreign-procedure sequence-to-seq   sequence/sequence-to-seq)
(define-foreign-procedure to-immutable-list sequence/sequence-to-seq)
(define-foreign-procedure nth               sequence/metaprob-nth)
(define-foreign-procedure range             sequence/metaprob-range)
(define-foreign-procedure append            sequence/append)
(define-foreign-procedure set-difference    sequence/set-difference)
(define-foreign-procedure sort              sequence/metaprob-sort)
(define-foreign-procedure to-list           sequence/to-list)
(define-foreign-procedure to-tuple          sequence/to-tuple)

;; Environments
(define-foreign-procedure top-level-lookup   impl/top-level-lookup)
(define-foreign-procedure make-top-level-env impl/make-top-level-env)
(define-foreign-procedure top-level-environment? trace/top-level-environment?)

;; Printing
(define-foreign-procedure print            impl/metaprob-print)
(define-foreign-procedure newline          trace/metaprob-newline)
(define-foreign-procedure pprint           trace/metaprob-pprint)
(define-foreign-procedure binned-histogram impl/binned-histogram)

;; Special procedures
(define-foreign-procedure inf         impl/inf)
(define-foreign-procedure infer-apply impl/infer-apply)

;; Deprecated
(define-foreign-procedure eq  clojure.core/=)
(define-foreign-procedure gt  clojure.core/>)
(define-foreign-procedure gte clojure.core/>=)
(define-foreign-procedure lte clojure.core/<=)
(define-foreign-procedure lt  clojure.core/<)
(define-foreign-procedure sub clojure.core/-)
(define-foreign-procedure mul clojure.core/*)
(define-foreign-procedure div clojure.core//)

;; -----------------------------------------------------------------------------
;; Work in progress

(define-foreign-procedure generate-foreign       impl/generate-foreign)
(define-foreign-procedure make-foreign-procedure impl/make-foreign-procedure)
(define-foreign-procedure foreign-procedure?     trace/foreign-procedure?)
(define-foreign-procedure procedure?             trace/procedure?)

(def positive-infinity Double/POSITIVE_INFINITY)
(def negative-infinity Double/NEGATIVE_INFINITY)
(define-foreign-procedure same-trace-states? trace/same-trace-states?)
(define-foreign-procedure same-states?       trace/same-states?)

(defmacro and [& forms]  `(clojure.core/and ~@forms))
(defmacro or [& forms]   `(clojure.core/or ~@forms))
(defmacro cond [& forms] `(clojure.core/cond ~@forms))
(defmacro case [& forms] `(clojure.core/case ~@forms))

;; --- kludge. based on clojure time macro.

(defn report-on-elapsed-time [tag thunk]
  (let [start (. System (nanoTime))
        ret   (thunk)
        t     (->> start
                   (- (. System (nanoTime)))
                   (double)
                   (* 1.0E-9)
                   (java.lang.Math/round))]
    (when (> t 1)
      (print (str tag ": elapsed time " t " sec\n")))
    ret))
