(ns rf-cljs.network.metrics
  (:refer-clojure :exclude [+ - * /])
  (:require [rf-cljs.network.params :as params]
            [rf-cljs.math.matrix :as mat]
            [rf-cljs.math.operations :refer [abs square + - * / sqrt]]))

(defn passivity [])

(defn reciprocity [s]
  (mat/matrix (for [i (range (first (mat/shape s)))
                    :let [s (mat/squeeze (mat/idx s i :all :all))]]
                (- s (mat/transpose s)))))

(defn passive? [])

(defn active? [s])

(defn reciprocal? [])

(defn -per-f [f m]
  (case (count (mat/shape m))
    2 (f m)
    3 (mat/broadcast m f 0)))

(defn -destructure-s [s]
  (assert (params/two-port? s) "S-Parameters must be two port")
  [(mat/idx s 0 0) (mat/idx s 0 1)
   (mat/idx s 1 0) (mat/idx s 1 1)])

(defn delta
  [s]
  (-per-f #(let [[S11 S12 S21 S22] (-destructure-s %)]
             (- (* S11 S22) (* S12 S21)))
          s))

(defn stab-fact
  "Stability factor (K)"
  [s]
  (-per-f #(let [[S11 S12 S21 S22] (-destructure-s %)]
             (/ (- 1 (square (abs S11)) (square (abs S22)) (square (abs (delta %))))
                (* 2 (abs (* S21 S12)))))
          s))

(defn max-avail-gain
  [s]
  (-per-f #(let [[_ S12 S21 _] (-destructure-s %)
                 K (stab-fact %)]
             (* (- K (sqrt (- (square K) 1))) (/ (abs S21) (abs S12))))
          s))

(defn max-stab-gain
  "Maximum stable gain"
  [s]
  (-per-f #(let [[_ S21 S12 _] (-destructure-s %)
                 K (stab-fact %)]
             (* (/ (* 2 K)) (/ (abs S21) (abs S12))))
          s))
