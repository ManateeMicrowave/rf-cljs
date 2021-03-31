(ns rf-cljs.network.core
  "Collection of functions which operate on network maps"
  (:refer-clojure :exclude [+ - * /])
  (:require
   [goog.string :as gstring]
   [goog.string.format]
   [clojure.string :as str]
   [rf-cljs.network.params :as params]
   [rf-cljs.math.matrix :as mat]
   [rf-cljs.math.operations :as op]
   [rf-cljs.math.operations :refer [abs square + - * / sqrt]]))

(defn -interp-to-s-linear
  [net desired-freq] ; TODO implement 
  (let [s (:s net)
        freq (:freq net)
        n-desired-freqs (count desired-freq)]
    (loop [i-d 0 ;Index of the desired frequency that we are going to interpolate on
           i-c 0 ;Index of the `from` frequency range. 
           s-output []]
      (if-not (= i-d n-desired-freqs)
      ; If we haven't gone past the number of desired frequencies
        (let [f-des (nth desired-freq i-d)
              f-lo (nth freq i-c)
              f-hi (nth  freq (inc i-c))]
          (if-not (and (op/lt-approx f-lo f-des) (op/lt-approx f-des f-hi))
          ; If `f-des` isn't in between `f-lo` and `f-hi` 
            (recur i-d (inc i-c) s-output)
            ; Select next two `from` points to see if desired is between
            ((let [ratio (/ (- f-des f-lo) (- f-hi f-lo))
                   s-vec-lo (params/-destructure-two-port (mat/idx s i-c))
                   s-vec-hi (params/-destructure-two-port (mat/idx s (inc i-c)))
                   s-interp (mat/reshape (mat/matrix  (apply #(+ (* ratio %1) (* (- 1 ratio) %2)) (map vector s-vec-lo s-vec-hi))) [2 2])]
               (recur (inc i-d) i-c (conj s-output s-interp))
               ; Use ratio to linearily interpolate between real and complex parts 
               ; of each s parameter in nth matrix
               ))))
        {:s s-output
         :freq desired-freq
         :z0 (:z0 net)}))))
         ; Exit condition, return hopefully completed interpolated net map

(defn -interp-to-s-cubic
  [net desired-freq] ; TODO implement 
  (println (str net desired-freq))
  nil)

(def -interp-method-map
  {:linear -interp-to-s-linear
   :cubic -interp-to-s-cubic})

(defn interpolate
  "Interpolate the response of the `rf-cljs/network` passed as `from` 
   onto the frequencies defined by `(:freq onto)`
   "
  ([net desired-freq] (interpolate net desired-freq :linear))
  ([net desired-freq method]
   (let ([onto_f0 (first desired-freq)
          onto_fn (last desired-freq)
          from_f0 (first (:freq from))
          from_fn (last (:freq from))]
         (if (and
              (op/lt-approx onto_f0 from_f0)
              (op/lt-approx from_fn onto_f0))
           ({:s ((method -interp-method-map) net desired-freq)
             :freq desired-freq
             :z0 (:z0 net)})
           (assert false (str "Could not map `from` network with frequency range "
                              from_f0 " to " from_fn " onto `onto` frequency range "
                              onto_f0 " to " onto_fn " .")))))))
