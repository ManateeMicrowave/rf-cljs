(ns rf-cljs-test
  (:require
   [cljs.test :refer-macros [deftest is]]
   [rf-cljs.networks.network :as network]
   [rf-cljs.math.complex :as cplx]
   [rf-cljs.math.matrix :as mat]))

(def networks [:abcd :z :y :s :t :h])
(def test-network-size [100 2 2])

(deftest round-trip-network
  (let [abcd (mat/random-complex test-network-size)]
    (doseq [net networks
            z0 [50 (cplx/complex 50 25)]
            :let [to (network/convert {:from :abcd :to net :data abcd :z0 z0})
                  from (network/convert {:from net :to :abcd :data to :z0 z0})]]
      (is (mat/equals abcd from)))))

(deftest renomalize
  (let [s (mat/random-complex test-network-size)
        z75 (network/renormalize s 50 75)]
    (is (mat/equals s (network/renormalize z75 75 50)))))

(def param-map 
  "Map of parameter keywords to parameters, all corresponding to a two port network consisting
   of a shunt -50jΩ reactance and series 50 Ω resistance. 2x2 parameter matrix is repeated twice
   at top dimension to test "
  {
   :z (mat/matrix [
                   [[(cplx/complex 0 -50) (cplx/complex 0 -50)] [(cplx/complex 0 -50) (cplx/complex 50 -50)]]
                   [[(cplx/complex 0 -50) (cplx/complex 0 -50)] [(cplx/complex 0 -50) (cplx/complex 50 -50)]]
                   ])
   :y (mat/matrix [
                   [[(cplx/complex 0.02 0.02) (cplx/complex -0.02 0)] [(cplx/complex -0.02 0) (cplx/complex 0.02 0)]]
                   [[(cplx/complex 0.02 0.02) (cplx/complex -0.02 0)] [(cplx/complex -0.02 0) (cplx/complex 0.02 0)]]
                   ])
   })

(deftest z2y
  (is (mat/equals (:z param-map) (network/convert {:from :y :to :z :data (:y param-map) :z0 50}))))