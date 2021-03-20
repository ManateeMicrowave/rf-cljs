(ns rf-cljs.core
  (:require
   [clojure.spec.alpha :as s]
   [clojure.edn :as edn]
   [rf-cljs.math.matrix :as mat]
   [rf-cljs.math.complex :as cplx]))

;; On its own, an s matrix must be square
(s/def ::s (s/and mat/matrix?
                  #(let [shape (mat/shape %)]
                     (if (= (count shape) 2)
                       (= (nth shape 0) (nth shape 1))
                       (= (nth shape 1) (nth shape 2))))))

;; Z0 is single port (complex or real) or a collextion of complexes and reals
(s/def ::z0 (s/or
             :single cplx/number?
             :multi (s/coll-of cplx/number?)))

;; Frequency is a single point, or a collection of distinct frequencies (real)
(s/def ::freq (s/or
               :single cplx/real?
               :multi (s/coll-of cplx/real? :distinct true)))

(s/def ::network
  (s/and (s/keys :req [::s
                       ::z0
                       ::freq])
         ;; Multiport Z0 must match nports from s
         #(if (= :multi (first (::z0 %)))
            (= (count (second (::z0 %))) (second (mat/shape (::s %))))
            true)
         ;; Number of frequencies must match length of first axis of s
         #(let [[x _ z] (mat/shape (::s %))]
            (if (or (nil? z) ; (shape s) -> [n n]
                    (= x 1)) ; (shape s) -> [1 n n]
              (= :single (first (::freq %))) ; one frequency
              (= (count (second (::freq %))) x)))))

(s/conform ::network {::s (mat/random-complex [3 2 2])
                      ::z0 [50 (cplx/complex 1 1)]
                      ::freq [1 2 3]})
