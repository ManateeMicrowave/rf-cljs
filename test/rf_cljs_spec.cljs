(ns rf-cljs-spec
  (:require
   [cljs.test :refer-macros [deftest is]]
   [rf-cljs.networks.network :as network]
   [rf-cljs.math.matrix :as mat]))

(def networks [:abcd :z :y :s :t :h])
(def test-network-size [100 2 2])

(deftest round-trip-network
  (let [abcd (mat/random test-network-size)]
    (for [net networks
          :let [to (network/convert {:from :abcd :to net :data abcd :Z0 50})
                from (network/convert {:from net :to :abcd :data to :Z0 50})]]
      (is (= abcd from)))))
