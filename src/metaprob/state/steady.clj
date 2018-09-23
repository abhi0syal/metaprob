(ns metaprob.state.steady)

(declare state-to-map keys-sans-value)

(def rest-marker "rest")

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
  (keys-sans-value (state-to-map state)))

(defn ^:private keys-sans-value [m]   ;aux for above
  (let [ks (remove #{:value} (keys m))]
    (if (= ks nil)
      '()
      ks)))

(defn subtrace-count [state]
  (count (state-keys state)))

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

        true (assert false ["not a state" state])))
