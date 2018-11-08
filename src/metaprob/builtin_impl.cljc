;; This module is intended for import by metaprob code.
;; To be used in conjunction with the syntax module.

(ns metaprob.builtin-impl
  #?(:clj (:import [java.lang Math]
                   [java.util Random]))
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.set :as set]
            [metaprob.trace :as trace]
            [metaprob.sequence :as sequence]))

;; -----------------------------------------------------------------------------
;; Addresses

;; addr - create an address out of a key sequence

(declare procedure-name)

(defn addr [& keys]
  (if (= keys nil)
    '()
    (map (fn [key]
           (if (trace/procedure? key)
             (procedure-name key)
             (do (assert (trace/ok-key? key))
                 key)))
         keys)))

;; Translation of .sites method from trace.py.
;; Returns a seq of addresses, I believe.  (and addresses are themselves seqs.)

(defn addresses-of [tr]
  (letfn [(get-sites [tr]
            ;; returns a seq of traces
            (let [site-list
                  (mapcat (fn [key]
                            (map (fn [site]
                                   (cons key site))
                                 (get-sites (trace/trace-subtrace tr key))))
                          (trace/trace-keys tr))]
              (if (trace/trace-has? tr)
                (cons '() site-list)
                site-list)))]
    (let [s (get-sites tr)]
      (doseq [site s]
        (assert (trace/trace-has? tr site) ["missing value at" site]))
      s)))

;; -----------------------------------------------------------------------------
;; Control

;; In metaprob, these are strict functions.

(defn neq [x y] (not (= x y)))

;; Procedure-related

;; Invoke a "foreign" procedure.  Called from interpreter.

(defn generate-foreign [ifn inputs]
  (assert (trace/proper-function? ifn) ["not a foreign-procedure" ifn inputs])
  (apply ifn (sequence/sequence-to-seq inputs)))

(defn make-foreign-procedure [name ifn]
  (assert (trace/proper-function? ifn) ["not procedure" name ifn])
  ifn)

;; -----------------------------------------------------------------------------
;; Misc

;; Like to-immutable, but recursive.
;; DEPRECATED, DO NOT USE.

(defn ^:private freeze [x]
  (if (trace/trace? x)
    (let [x (trace/trace-state x)]
      (cond (seq? x)
            (map freeze x)

            (vector? x)
            (vec (map freeze (seq x)))

            true
            (let [keys (trace/trace-keys x)
                  result (into {} (for [key keys] [key (freeze (trace/trace-subtrace x key))]))]
              (if (trace/trace-has? x)
                (assoc result :value (freeze (trace/trace-get x)))
                result))))
    x))

;; This is for computing a name for a procedure at its point of creation.
;; Careful, this loses if there's a cycle.  Don't include a lexical
;; environment in tr.

(defn trace-name
  ([proc-parse-tree]
   (str (hash (freeze proc-parse-tree))))
  ([proc-parse-tree name]
   (if name
     (str name "-" (trace-name proc-parse-tree))
     (trace-name proc-parse-tree))))

(defn foreign-procedure-name [ifn]
  (str ifn))

;; Mainly for foreign procedures.

(defn procedure-name [pp]
  (if (trace/trace? pp)
    (if (trace/trace-has? pp "name")
      (trace/trace-get pp "name")          ;Cached
      (str "?-" (trace-name pp)))    ;???
    (foreign-procedure-name pp)))    ;E.g. "clojure.core$str@1593f8c5"

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly

;; What about sp = tracing_proposer_to_prob_prog in prelude (!!) - do
;; we need it, how to define, etc.?

;; original prelude has: proposer_of factor apply_with_address

;; prelude has: trace_of lookup_chain lookup_chain_with_exactly

;; The assert macro in clojure is much nicer, since (1) it can catch
;; exceptions in the evaluation of its subforms, (2) it can show you
;; the source code for the subforms, (3) it doesn't evaluate the
;; 2nd argument unless the condition fails.

(defn metaprob-assert [condition complaint & irritants]
  (binding #?(:clj [*out* *err*]
              :cljs [*print-fn* *print-err-fn*])
    (doseq [irritant irritants]
      (if (trace/mutable-trace? irritant)
        (do (print "Irritant:")
            (trace/metaprob-pprint irritant)))))
  (assert condition
          (if (empty? irritants)
            complaint
            (vec (cons complaint irritants)))))

;; error - overrides original prelude (???)

(defn error [& irritants]
  (assert false irritants))                     ;from prelude.vnts

;; ----------------------------------------------------------------------------
;; Metaprob top level environments are represented as clojure namespaces.

#?(:clj
   (defn make-top-level-env [ns]
     (let [ns (if (symbol? ns)
                (find-ns ns)
                ns)]
       (assert (trace/top-level-environment? ns))
       ns)))

;; TBD: extend this to allow namespace-prefixed variable references foo/bar

(defn top-level-lookup [the-ns name]
  (assert (string? name) ["wanted a string" the-ns name])
  (assert (trace/top-level-environment? the-ns) ["wanted a top-level env" the-ns name])
  (let [v #?(:clj (ns-resolve the-ns (symbol name))
             :cljs (ns-resolve (some-ns/analyzer-state)))]
    (assert (var? v) ["no such variable" the-ns name])
    (assert (not (get (meta v) :macro)) ["reference to macro" the-ns name])
    (assert (bound? v) ["unbound variable" the-ns name])
    (deref v)))

#?(:clj
   (defn top-level-bind! [the-ns name value]
     ;; how to create a new binding in a namespace (a la def)???
     (let [sym (symbol name)
           r (ns-resolve the-ns sym)
           r (if r r (binding [*ns* the-ns]
                       (print "Assigning" sym "in" the-ns)
                       (eval `(def ~sym))
                       (ns-resolve the-ns sym)))]
       (ref-set r value)
       nil)))

;; ----------------------------------------------------------------------------
;; Mathematical

(defn exp [x]
  #?(:clj (Math/exp x)
     :cljs (js/Math.exp x)))

(defn expt [x y]
  #?(:clj (Math/pow x y)
     :cljs (js/Math.pow x y)))

(defn sqrt [x]
  #?(:clj (Math/sqrt x)
     :cljs (js/Math.sqrt x)))

(defn add
  ([x y]
   (if (number? x)
     (+ x y)
     (if (and (string? x) (string? y))
       (str x y)
       (let [x (if (string? x) (list x) x)
             y (if (string? y) (list y) y)]
         (if (and (trace/trace? x) (trace/trace? y))
           (sequence/append x y)
           (assert false ["not addable" x y]))))))
  ([] 0)
  ([x] x)
  ;; this is rather foolish
  ([x y z & w] (add (add x y) (apply add z w))))

(defn log [x]
  #?(:clj (Math/log x)
     :cljs (js/Math.log x)))

(defn cos [x]
  #?(:clj (Math/cos x)
     :cljs (js/Math.cos)))

(defn sin [x]
  #?(:clj (Math/sin x)
     :cljs (js/Math.sin x)))

(defn log1p [x]
  #?(:clj (Math/log1p x)
     :cljs (js/Math.log1p x)))

(defn floor [x]
  #?(:clj (Math/floor x)
     :cljs (js/Math.floor x)))

(defn round [x]
  #?(:clj (Math/round x)
     :cljs (js/Math.round x)))

(def pi*2 (* 2 #?(:clj (Math/acos -1)
                  :cljs (js/Math.acos -1))))
(defn normal [mu variance]              ;not needed any more?
  (fn [x]
    (let [x-mu (- x mu)]
      (/ (exp (- 0 (/ (* x-mu x-mu) (* 2.0 variance))))
         (sqrt (* pi*2 variance))))))

#?(:clj (def ^:dynamic *rng*
          (Random. 42)))

(defn- next-random
  []
  #?(:clj (.nextDouble *rng*)
     :cljs (js/Math.random)))

(defn sample-uniform
  ([] (next-random))
  ([a b] (+ a (* (next-random) (- b a)))))


;; -----------------------------------------------------------------------------
;; Graphical output (via gnuplot or whatever)

#?(:clj (defn binned-histogram [& {:keys [name samples overlay-densities
                                          sample-lower-bound sample-upper-bound
                                          number-of-intervals]}]
          (let [samples (sequence/sequence-to-seq samples)
                sample-lower-bound (or sample-lower-bound -5)
                sample-upper-bound (or sample-upper-bound 5)
                number-of-intervals (or number-of-intervals 20)
                fname (clojure.string/replace name " " "_")
                path (str "results/" fname ".samples")
                commands-path (str path ".commands")]
            (print (format "Writing commands to %s for histogram generation\n" commands-path))
            ;;(print (format " overlay-densities = %s\n" (freeze overlay-densities)))
            (with-open [writor (io/writer commands-path)]
              (.write writor (format "reset\n"))
              (.write writor (format "min=%s.\n" sample-lower-bound))
              (.write writor (format "max=%s.\n" sample-upper-bound))
              (.write writor (format "n=%s\n" number-of-intervals))
              (.close writor))
            (print (format "Writing samples to %s\n" path))
            (with-open [writor (io/writer path)]
              (doseq [sample samples]
                (.write writor (str sample))
                (.write writor "\n"))
              (.close writor)))))

;; Maybe this should print (i.e. princ) instead of pr (i.e. prin1)?

;;!!
(defn metaprob-print [x]
  (print x)
  (newline)
  (flush))

;; -----------------------------------------------------------------------------
;; Interpreter-related

;; This could go in prelude.clj, with some effort.

(defn inf [name model implementation]
  (assert (trace/procedure? implementation) implementation)
  (trace/trace-as-procedure (trace/mutable-trace "name" (str "inf-" name)
                                                 "model" model
                                                 "implementation" implementation)
                            ;; When called from Clojure:
                            (fn [& inputs]
                              (let [inputs (if (= inputs nil) (list) inputs)]
                                (nth (implementation inputs (trace/trace) (trace/trace) false)
                                     0)))))

(defn clojure-interpreter [proc inputs
                           intervention-trace target-trace output-trace?]
  (assert (trace/foreign-procedure? proc))
  (assert (trace/empty-trace? intervention-trace))
  (assert (trace/empty-trace? target-trace))
  (assert (not output-trace?))
  [(clojure.core/apply proc inputs) (trace/trace) 0])

(def ^:dynamic *ambient-interpreter* clojure-interpreter)

(defn infer-apply [proc inputs intervention-trace
                   target-trace output-trace?]
  (if (and (trace/foreign-procedure? proc)
           (trace/empty-trace? intervention-trace)
           (trace/empty-trace? target-trace)
           (not output-trace?))
    ;; Bypass inference when there is no need to use it.
    [(generate-foreign proc inputs) (trace/trace) 0]

    (let [[value output score]
          (*ambient-interpreter* proc inputs intervention-trace
                                 target-trace output-trace?)]
      (assert (number? score) ["bad score"
                               score proc
                               (if (trace/trace? *ambient-interpreter*)
                                 (trace/trace-state *ambient-interpreter*)
                                 *ambient-interpreter*)])
      (assert (if output-trace?
                (trace/trace? output)
                true)
              ["bad output" output proc])
      [value output score])))
