(ns rf-cljs.network.metrics
  (:require [rf-cljs.network.params :as params]))

(defn passivity [])

(defn reciprocity [s]
  (mat/matrix (for [i (range (first (mat/shape s)))
                    :let [s (mat/squeeze (mat/idx s i :all :all))]]
                (- s (mat/transpose s)))))

(defn passive? [])

(defn active? [s])

(defn reciprocal? [])

(defn stab-fact
  "Stability factor (K)"
  [s]
  (assert (params/two-port? s) "S-Parameters must be two port"))

(defn stab-meas
  "Stability measure (delta)"
  [s]
  (assert (params/two-port? s) "S-Parameters must be two port"))

(defn gmax
  "Maximum available gain"
  [s]
  (assert (params/two-port? s) "S-Parameters must be two port"))

(defn msg
  "Maximum stable gain"
  [s]
  (assert (params/two-port? s) "S-Parameters must be two port"))
