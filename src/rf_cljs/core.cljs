(ns rf-cljs.core
  (:refer-clojure :exclude [+ * /])
  (:require
   [clojure.spec.alpha :as s]
   ["mathjs" :as mathjs]
   [rf-cljs.network.params :as params]
   [rf-cljs.math.operations :refer [acos cos square + hypot norm log10 abs * /]]
   [rf-cljs.math.matrix :as mat]
   [rf-cljs.math.complex :as cplx]))

(s/def ::s (s/and mat/matrix?
                  #(let [shape (mat/shape %)]
                     (if (= (count shape) 2)
                       (= (nth shape 0) (nth shape 1))
                       (= (nth shape 1) (nth shape 2))))))

(s/def ::z0 (s/or
             ::single-complex cplx/complex?
             ::single-real number?
             ::multi-vec (s/coll-of (s/or
                                     :real (s/and number?
                                                  pos?)
                                     :complex cplx/complex?))
             ::multi-mat mat/matrix?))

(s/def ::freq (s/or
               :single-freq number?
               :multi-freq (s/coll-of number? :distinct true)))

(s/def ::network
  (s/and (s/keys :req [::s
                       ::z0
                       ::freq])
         #(let [[_ n _] (mat/shape (::s %))]
            (if (vector? (::z0 %))
              (= (count (::z0 %)) n)
              (if (mat/matrix? (::z0 %))
                (= [1 n] (mat/shape (::z0 %)))
                (or (number? (::z0 %))
                    (cplx/complex? (::z0 %))))))
         #(let [shape (mat/shape (::s %))
                dim (count shape)
                length (if (= dim 2)
                         1
                         (first shape))]
            (if (= length 1)
              (number? (::freq %))
              (= (count (::freq %)) length)))))

(s/valid? ::s (mat/matrix [[1 2] [3 4]]))
(s/valid? ::z0 [50 (cplx/complex 1 1)])
(s/valid? ::freq (range 10))
(s/valid? ::freq [1 1 2])

(s/valid? ::network {::s (mat/matrix [[1 2] [3 4]])
                     ::z0 50
                     ::freq 1e9})
