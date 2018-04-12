;; 3.

(ns metaprob.examples.importance
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.examples.interpreters :refer :all]))

(define importance-resampling
  (gen [model-procedure inputs target-trace N]

    ;; generate N candidate traces, called particles, each
    ;; with a score
    
    (define particles
    	    (replicate N
	      (gen []
                (define candidate-trace (empty-trace))
                (define [_ score]
                  (infer    ;; returns [value score]
                   :procedure model-procedure
                   :inputs inputs
                   :intervention-trace (empty-trace)
                   :target-trace       target-trace
                   :output-trace       candidate-trace))
                (tuple candidate-trace score))))
    (define traces 
      (map (gen [p] (trace-get p 0)) particles))
    (define scores
      (map (gen [p] (trace-get p 1)) particles))

    ;; return a trace with probability proportional to (exp score)
    (define which (log-categorical scores))
    (nth traces which)))