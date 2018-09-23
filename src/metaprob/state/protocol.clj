(ns metaprob.state.protocol
  (:import [clojure.lang IPersistentVector IPersistentList IPersistentMap ISeq]))

(def rest-marker "rest")

(defn- keys-sans-value [m]   ;aux for above
  (let [ks (remove #{:value} (keys m))]
    (if (= ks nil)
      '()
      ks)))

(defprotocol IState
  (has-value? [state])
  (value [state])
  (has-subtrace? [state key])
  (subtrace [state key])
  (state-keys [state])
  (subtrace-count [state])
  (state-to-map [state])
  (value-only-trace? [tr]))

(extend-protocol IState
  ISeq
  (has-value? [state]
    (not (empty? state)))
  (value [state]
    (first state))
  (has-subtrace? [state key]
    (and (not (empty? state))
         (= key rest-marker)))
  (subtrace [state key]
    (let [val (rest state)]
      (assert (not (= val nil))
              ["no such subtrace" key state])
      val))
  (state-keys [state]
    (if (empty? state) '() (list rest-marker)))
  (subtrace-count [state]
    (if (empty? state) 0 1))
  (state-to-map [state]
    (if (empty? state)
      {}
      {:value (first state) rest-marker (rest state)}))
  (value-only-trace? [tr]
    false)

  IPersistentVector
  (has-value? [state]
    false)
  (value [state]
    (assert false "no value"))
  (has-subtrace? [state key]
    (and (integer? key)
         (>= key 0)
         (< key (count state))))
  (subtrace [state key]
    (let [val {:value (nth state key)}]
      (assert (not (= val nil))
              ["no such subtrace" key state])
      val))
  (state-keys [state]
    (range (count state)))
  (subtrace-count [state]
    (count state))
  (state-to-map [state]
    (into {} (map (fn [i x] [i {:value x}])
                  (range (count state))
                  state)))
  (value-only-trace? [tr]
    false)

  IPersistentMap
  (has-value? [state]
    (not (= (get state :value :no-value) :no-value)))
  (value [state]
    (let [value (get state :value :no-value)]
      (assert (not (= value :no-value)) ["state has no value" state])
      value))
  (has-subtrace? [state key] (contains? state key))
  (subtrace [state key]
    (let [val (get state key)]
      (assert (not (= val nil))
              ["no such subtrace" key state])
      val))
  (state-keys [state]
    (keys-sans-value state))
  (subtrace-count [state]
    (let [n (count state)]
      (if (= (get state :value :no-value) :no-value)
        n
        (- n 1))))
  (state-to-map [state]
    state)
  (value-only-trace? [tr]
    (= (set (keys tr)) #{:value})))

(defn state?
  [x]
  ;; satisfies? is slow https://dev.clojure.org/jira/browse/CLJ-1814
  (or (seq? x)
      (vector? x)
      (map? x)))
