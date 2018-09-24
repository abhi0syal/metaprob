;; Trace states

(ns metaprob.state)

(declare map-to-state state-to-map keys-sans-value)

;; The `steady?` flag controls a choice between a simple but slower
;; implementation and an obscure but faster implementation of the
;; primitives.

(def steady? false)

(def rest-marker "rest")

(defn state? [val]
  (or (seq? val)                        ;Strings are not seqs
      (vector? val)
      (map? val)))

;; Basic trace operations

(defn has-value? [state]
  (if steady?
    ;; The two methods should be equivalent
    (not (= (get (state-to-map state) :value :no-value) :no-value))
    (cond (seq? state) (not (empty? state))
          (vector? state) false
          (map? state) (not (= (get state :value :no-value) :no-value))
          :else (throw (ex-info "not a state" {:state state})))))

(defn value [state]
  (if steady?
    ;; The two methods should be equivalent
    (get (state-to-map state) :value)
    (cond (seq? state) (first state)
          (vector? state) (assert false "no value")
          (map? state)
          (if-not (has-value? state)
            (throw (ex-info "state has no value" state))
            (get state :value))
          :else (throw (ex-info "not a state" {:state state})))))

(defn has-subtrace? [state key]
  (if steady?
    (contains? (state-to-map state) key)
    (cond (seq? state) (and (not (empty? state))
                            (= key rest-marker))
          (vector? state) (and (integer? key)
                               (>= key 0)
                               (< key (count state)))
          (map? state) (contains? state key)
          :else (throw (ex-info "not a state" {:state state})))))

(defn subtrace [state key]
  (if-let [val (if steady?
                 (get (state-to-map state) key)
                 (cond (seq? state) (rest state)
                       (vector? state) {:value (nth state key)}
                       (map? state) (get state key)
                       :else (throw (ex-info "not a state" {:state state}))))]
    val
    (throw (ex-info "no such subtrace" {:state state :key key}))))

(defn state-keys [state]
  (if steady?
    (keys-sans-value (state-to-map state))
    (cond (seq? state) (if (empty? state) '() (list rest-marker))
          (vector? state) (range (count state))
          (map? state) (keys-sans-value state)
          :else (throw (ex-info "not a state" {:state state})))))

(defn ^:private keys-sans-value [m]   ;aux for above
  (let [ks (remove #{:value} (keys m))]
    (if (= ks nil)
      '()
      ks)))

(defn subtrace-count [state]
  (if steady?
    (count (state-keys state))
    (cond (seq? state) (if (empty? state) 0 1)
          (vector? state) (count state)
          (map? state) (let [n (count state)]
                         (if (= (get state :value :no-value) :no-value)
                           n
                           (- n 1)))
          :else (throw (ex-info "not a state" {:state state})))))

;; Constructors

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



;; Convert heterogeneous canonical clojure form to hash-map

(defn state-to-map [state]
  (cond (map? state) state

        (seq? state)
        (if (empty? state)
          {}
          {:value (first state) rest-marker (rest state)})

        (vector? state)
        (into {} (map (fn [i x] [i {:value x}])
                      (range (count state))
                      state))

        :else (throw (ex-info "not a state" {:state state}))))

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

          :else (if-not (map? m)
                  (throw (ex-info "expected a map" {:value m}))
                  (do (doseq [entry m] true)    ;Don't be lazy!
                      m)))))
