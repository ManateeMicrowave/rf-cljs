(ns rf-cljs-test
  (:require
   [cljs.test :refer-macros [deftest is testing]]
   [rf-cljs.networks.network :as network]
   [rf-cljs.math.complex :as cplx]
   [rf-cljs.math.matrix :as mat]))

(def networks [:abcd :z :y :s :t :h])
(def test-network-size [25 2 2])
(def eps 1e-10) ; epsilon used for floating point equality comparisons

(deftest round-trip-network
  (let [data (mat/random-complex test-network-size)]
    (doseq [neta networks
            netb networks
            z0 [50 (cplx/complex 50 75)]
            :when (not= neta netb)
            :let [to (network/convert {:from neta :to netb :data data :z0 z0})
                  from (network/convert {:from netb :to neta :data to :z0 z0})]]
      (testing (str "Roundtrip " neta " -> " netb " -> " neta ". Z0 = " z0)
        (is (mat/equals data from eps))))))

(deftest renomalize
  (let [s (mat/random-complex test-network-size)]
    (testing "Renomalize s to 75 and back"
      (is (mat/equals s (network/renormalize (network/renormalize s 50 75) 75 50)) eps))
    (testing "Renomalize complex and back"
      (is (mat/equals s (network/renormalize (network/renormalize s (cplx/complex 1 1) 50) 50 (cplx/complex 1 1)) eps)))))

(def param-map
  "Map of parameter keywords to parameters. 2x2 parameter matrix is repeated twice
   at top dimension to test "
  {:s     (mat/matrix [[[(cplx/complex 1 0) (cplx/complex -1 0)]
                        [(cplx/complex -1 1) (cplx/complex -1 -1)]]])
   :z     (mat/matrix [[[(cplx/complex -100 -150) (cplx/complex 50 50)]
                        [(cplx/complex 100 0) (cplx/complex -50 0)]]])
   :y     (mat/matrix [[[(cplx/complex 0 0.02) (cplx/complex -0.02 0.02)]
                        [(cplx/complex 0 0.04) (cplx/complex -0.06 0.04)]]])
   :abcd  (mat/matrix [[[(cplx/complex -1 -1.5) (cplx/complex 0 25)]
                        [(cplx/complex 0.01 0) (cplx/complex -0.5 0)]]])
   :h     (mat/matrix [[[(cplx/complex 0 -50) (cplx/complex -1 -1)]
                        [(cplx/complex 2 0) (cplx/complex -0.02 0)]]])
   :t     (mat/matrix [[[(cplx/complex -1 -1) (cplx/complex -0.5 -0.5)]
                        [(cplx/complex 0 -1) (cplx/complex -0.5 -0.5)]]])})

(deftest to-s-tests
  (doseq [net networks]
    (testing (str net)
      (is (mat/equals (:s param-map) (network/to-s {:from net :data (net param-map) :z0 50}) eps)))))

(deftest from-s-tests
  (doseq [net networks]
    (testing (str net)
      (is (mat/equals (net param-map) (network/from-s {:to net :data (:s param-map) :z0 50}) eps)))))

(deftest even-port
  (doseq [nports [4 6 8]
          :let [data (mat/random-complex [25 nports nports])]
          net [:s :y :z :t]
          z0 [50 (cplx/complex 50 75)]]
    (let [to (network/convert {:from net :to :s :data data :z0 z0})
          from (network/convert {:from :s :to net :data to :z0 z0})]
      (testing (str "Roundtrip s -> " net " -> s. Z0 = " z0)
        (is (mat/equals data from eps))))))

(deftest odd-port
  (doseq [nports [3 6 9]
          :let [data (mat/random-complex [25 nports nports])]
          net [:s :y :z]
          z0 [50 (cplx/complex 50 75)]]
    (let [to (network/convert {:from net :to :s :data data :z0 z0})
          from (network/convert {:from :s :to net :data to :z0 z0})]
      (testing (str "Roundtrip s -> " net " -> s. Z0 = " z0)
        (is (mat/equals data from eps))))))
