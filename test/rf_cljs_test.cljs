(ns rf-cljs-test
  (:refer-clojure :exclude [+ - * / < >])
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [rf-cljs.networks.network :as network]
   [rf-cljs.math.operations :refer [abs + - * /]]
   [rf-cljs.math.complex :as cplx]
   [rf-cljs.math.matrix :as mat]))

(def networks [:abcd :z :y :s :t :h])
(def test-network-size [1 2 2])
(def eps 0.000001) ; epsilon used for floating point equality comparisons

(deftest round-trip-network
  (let [abcd (mat/random-complex test-network-size)]
    (doseq [net networks
            z0 [50 (cplx/complex 50 25)]
            :let [to (network/convert {:from :abcd :to net :data abcd :z0 z0})
                  from (network/convert {:from net :to :abcd :data to :z0 z0})]]
      (testing (str "Roundtrip abcd -> " net " -> abcd")
        (is (mat/equals abcd from eps))))))

(deftest renomalize
  (let [s (mat/random-complex test-network-size)
        z75 (network/renormalize s 50 75)]
    (testing "Renomalize s to 75 and back"
      (is (mat/equals s (network/renormalize z75 75 50)) eps))))

(def param-map
  "Map of parameter keywords to parameters, all corresponding to a two port network consisting
   of a shunt -50jΩ reactance and series 50 Ω resistance. 2x2 parameter matrix is repeated twice
   at top dimension to test "
  {:s     (mat/matrix [[[(cplx/complex -0.07692308 -0.61538462) (cplx/complex 0.46153846 -0.30769231)]
                        [(cplx/complex 0.46153846 -0.30769231) (cplx/complex 0.23076923 -0.15384615)]]])
   :z     (mat/matrix [[[(cplx/complex 0 -50) (cplx/complex 0 -50)]
                        [(cplx/complex 0 -50) (cplx/complex 50 -50)]]])
   :y     (mat/matrix [[[(cplx/complex 0.02 0.02) (cplx/complex -0.02 0)]
                        [(cplx/complex -0.02 0) (cplx/complex 0.02 0)]]])
   :abcd  (mat/matrix [[[(cplx/complex 1 0) (cplx/complex 50 0)]
                        [(cplx/complex 0 0.02) (cplx/complex 1 1)]]])
   :h     (mat/matrix [[[(cplx/complex -5.19575693e+01 -2.07993441e+00) (cplx/complex -3.99833605e-02 -8.15660555e-04)]
                        [(cplx/complex 3.99833605e-02 8.15660555e-04) (cplx/complex -1.92000065e-02 3.19866884e-07)]]])
   :t     (mat/matrix [[[(cplx/complex 0.5 0) (cplx/complex 0.5 -1)]
                        [(cplx/complex -0.5 0) (cplx/complex 1.5 1)]]])})

(deftest to-s-tests
  (doseq [net networks]
    (testing (str net)
      (is (mat/equals (:s param-map) (network/to-s {:from net :data (net param-map) :z0 50}) eps)))))

(deftest from-s-tests
  (doseq [net networks]
    (testing (str net)
      (is (mat/equals (net param-map) (network/from-s {:to net :data (:s param-map) :z0 50}) eps)))))



