(ns metaprob.state.core)

(def rest-marker "rest")

(defn keys-sans-value [m]
  (let [ks (remove #{:value} (keys m))]
    (if (= ks nil)
      '()
      ks)))

(defn value-only-trace? [tr]
  (and (map? tr)
       (= 1 (count tr))
       (contains? tr :value)))

(defn map-to-state
  "Convert hash-map to heterogeneous canonical clojure form."
  [m]
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
