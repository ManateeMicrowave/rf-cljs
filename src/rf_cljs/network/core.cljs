(ns rf-cljs.network.core
  "Collection of functions which operate on network maps"
  (:refer-clojure :exclude [+ - * /])
  (:require [rf-cljs.network.params :as params]
            [rf-cljs.math.matrix :as mat]
            [rf-cljs.math.operations :as op]
            [rf-cljs.math.operations :refer [abs square + - * / sqrt]]))

(defn -interp-to-s-linear
  [net desired-freq] ; TODO implement 
  (let [s (:s net)
        freq (:freq net)
        n-desired-freqs (count desired-freq)]
    (loop [i_d 0 ;Index of the desired frequency that we are going to interpolate on
           i_c 0 ;Index of the `from` frequency range. 
           ]
      (if-not (= (inc i_d))
        (let [f_des (nth i_d desired-freq)
              f_lo (nth i_c freq)
              f_hi (nth (inc i_c) freq)]
          (if-not 
           (and
              (op/lt-approx f_lo f_des)
              (op/lt-approx f_des f_hi))
           (recur i_d (inc i_c)) ; Select next two `from` points to see if desired is between
           (
             (let [ratio (/ (- f_des f_lo) (- f_hi f_lo) )]
               ; Use ratio to linearily interpolate between real and complex parts 
               ; of each s parameter in nth matrix
               )
           ) ; Do interpolation
            ))
        ; () exit condition 
        )
      ))
)

(def -interp-method-map
  {:linear -interp-to-s-linear
  :cubic -interp-to-s-cubic
   })

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
