;; 5.

;; Try this: time lein run -m metaprob.examples.main 10

(ns metaprob.examples.inference-on-gaussian
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all])
  (:require [metaprob.prelude :refer :all])
  (:require [metaprob.interpreters :refer :all])
  (:require [metaprob.inference :refer :all])
  (:require [metaprob.examples.gaussian :refer [gaussian score-gaussian two-variable-gaussian-model]]))

;; Exact versions of prior and target density functions, for
;; graphical comparison with sampled approximations.

(define prior-density
  (gen [x]
    (exp (score-gaussian x [0 1]))))

(define target-density
  (gen [x]
    (exp (score-gaussian x [1.5 (/ 1.0 (sqrt 2.0))]))))

;; Each sample is an output trace.

;; Find the location of the (assumed unique) peak of the histogram.
;; For debugging.

(define peak-location
  (gen [samples]
    (define so (sort samples))
    (define window (+ 1 (clojure.core/quot (count so) 10)))
    (define nthcdr (gen [x i] (if (= i 0) x (nthcdr (rest x) (- i 1)))))
    (define lead (nthcdr so window))
    (nth (first (sort (clojure.core/map (gen [x y] [(- y x) (/ (+ x y) 2)])
                                        so
                                        lead)))
         1)))

;; For debugging.

(define analyze
  (gen [samples]
    (print (first samples))
    (print ["average:" (/ (apply clojure.core/+ samples) (count samples))
            "peak:" (peak-location samples)])
    samples))

(define gaussian-histogram
  (gen [name samples]
    (binned-histogram
      :name    name
      :samples (analyze samples)
      :overlay-densities `(["prior" ~prior-density] ["target" ~target-density]))))

;; Sample from prior & plot

(define gaussian-prior-samples
  (gen [number-of-runs]
    (replicate number-of-runs two-variable-gaussian-model)))

(define target-trace
  (trace-set-value {} '(1 "y" "gaussian") 3.0))

(define gaussian-sample-value
  (gen [output-trace]
    (trace-value output-trace '(0 "x" "gaussian"))))

(define rejection-assay
  (gen [number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (print "rejection sample") ;Progress meter
       (gaussian-sample-value 
        (rejection-sampling two-variable-gaussian-model  ; :model-procedure 
                            []  ; :inputs 
                            target-trace
                            0.5))))))   ; :log-bound 

(define importance-assay
  (gen [n-particles number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (gaussian-sample-value
        (importance-resampling two-variable-gaussian-model  ; :model-procedure 
                               []  ; :inputs 
                               target-trace
                               n-particles))))))

(define MH-assay
  (gen [count number-of-runs]
    (replicate
     number-of-runs
     (gen []
       (gaussian-sample-value
        (lightweight-single-site-MH-sampling two-variable-gaussian-model
                                             []
                                             target-trace
                                             count))))))
