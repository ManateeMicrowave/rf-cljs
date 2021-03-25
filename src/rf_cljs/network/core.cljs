(ns rf-cljs.network.core
  "Collection of functions which operate on network maps"
  (:refer-clojure :exclude [+ - * /])
  (:require [rf-cljs.network.params :as params]
            [rf-cljs.math.matrix :as mat]
            [rf-cljs.math.operations :as op]
            [rf-cljs.math.operations :refer [abs square + - * / sqrt]]))

(defn interp-one-freq-linear
  [net f]) ; TODO implement

(def interp-method-map
  {:linear interp-one-freq-linear})

(defn interpolate
  "Interpolate the response of the `rf-cljs/network` passed as `from` 
   onto the frequencies defined by `(:freq onto)`"
  ([net desired-freq] (interpolate net desired-freq :linear))
  ([net desired-freq method]
   (let ([onto_f0 (first desired-freq)
          onto_fn (last desired-freq)
          from_f0 (first (:freq from))
          from_fn (last (:freq from))]
         (if (and
              (op/lt-approx onto_f0 from_f0)
              (op/lt-approx from_fn onto_f0))
           ({:s (for [f desired-freq]
                  ((:linear interp-method-map) net f))
             :freq desired-freq
             :z0 (:z0 net)})
           (assert false (str "Could not map `from` network with frequency range "
                              from_f0 " to " from_fn " onto `onto` map with frequency range "
                              onto_f0 " to " onto_fn " .")))))))
