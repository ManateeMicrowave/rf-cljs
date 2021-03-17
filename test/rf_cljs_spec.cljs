(ns rf-cljs-spec
  (:require
   [cljs.test :refer-macros [deftest is]]
   [rf-cljs.networks.network :as network]
   [rf-cljs.math.complex :as complex]
   [rf-cljs.math.matrix :as mat]))

(def networks [:abcd :z :y :s :t :h])
(def test-network-size [100 2 2])

(deftest round-trip-network
  (let [abcd (mat/random-complex test-network-size)]
    (doseq [net networks
            z0 [50 (complex/complex 50 25)]
            :let [to (network/convert {:from :abcd :to net :data abcd :z0 z0})
                  from (network/convert {:from net :to :abcd :data to :z0 z0})]]
      (is (mat/equals abcd from)))))
