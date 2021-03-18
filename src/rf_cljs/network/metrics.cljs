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

(defn stab-fact [s]
  (assert (params/two-port? s) "S-Parameters must be two port"))
