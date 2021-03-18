(ns rf-cljs.networks.network
  (:refer-clojure :exclude [+ - * /])
  (:require
   [rf-cljs.math.operations :refer [+ - * / abs sqrt]]
   [rf-cljs.math.complex :as cmplx]
   [rf-cljs.math.matrix :as mat]
   ["mathjs" :as mathjs]))


;; (defmulti abcd :type)

;; (defmethod abcd :series [{:keys [Z]}]
;;   (matrix [[1 Z] [0 1]]))

;; (defmethod abcd :shunt [{:keys [Y]}]
;;   (matrix [[1 0] [Y 1]]))

;; (defmethod abcd :tee [{:keys [Za Zb Zc]}]
;;   (matrix [[(+ 1 (/ Za Zc)) (+ Za Zb (/ (* Za Zb) Zc))]
;;            [(/ 1 Zc) (+ 1 (/ Zb Zc))]]))

;; (defmethod abcd :pi [{:keys [Ya Yb Yc]}]
;;   (matrix [[(+ 1 (/ Yb Yc)) (/ 1 Yc)]
;;            [(+ Ya Yb (/ (* Ya Yb) Yc)) (+ 1 (/ Yb Yc))]]))

;; (defmethod abcd :tline [{:keys [z0 beta l]}]
;;   (let [j (mathjs/complex "0+1i")
;;         theta (* beta l)]
;;     (matrix [[(cos theta) (* j z0 (sin theta))]
;;              [(/ (* j (sin theta)) z0) (cos theta)]])))

;; (defmethod abcd :transformer [{:keys [N]}]
;;   (matrix [[N 0] [0 (/ N)]]))


(defn fix-z0-shape [z0 nports]
  (if (or (mat/matrix? z0)
          (vector? z0))
    (let [z0 (mat/matrix [z0])]
      (assert (= [nports] (mat/shape z0)) "(count z0) must equal nports")
      z0)
    (* z0 (mat/ones [nports]))))

(defn internal-external-partition
  "Partition the matrix of 2n ports into internal-external blocks
  Returns [ee ei ie ii]
  http://www.microwave.fr/publications/151.pdf"
  [data]
  (let [[m n] (mat/shape data)
        part-n (dec (quot n 2))]
    (assert (= m n) "Matrix must be square")
    (assert (= 0 (mod m 2)) "Matrix must be 2nx2n")
    (if (= n 2)
      [(mat/idx data 0 0)
       (mat/idx data 0 1)
       (mat/idx data 1 0)
       (mat/idx data 1 1)]
      [(mat/idx data [0 part-n] [0 part-n])
       (mat/idx data [0 part-n] [(inc part-n) (dec n)])
       (mat/idx data [(inc part-n) (dec n)] [0 part-n])
       (mat/idx data [(inc part-n) (dec n)] [(inc part-n) (dec n)])])))

(defmulti to-s :from)

; N-port parameters
(defmethod to-s :s [{:keys [data z0]}] data)

(defmethod to-s :z [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)]
    (assert (= nportsa nportsb) "Matrix must be square")
    (mat/matrix (for [i (range nfreqs)
                      :let [Z (mat/squeeze (mat/idx data i :all :all))]]
                  (let [G (mat/diag z0)
                        F (mat/diag (-> (cmplx/real z0)
                                        abs
                                        sqrt
                                        (* 0.5)))]
                    (* F (- Z (mat/ctranspose G)) (mat/inv (+ Z G)) (mat/inv F)))))))

(defmethod to-s :y [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)]
    (assert (= nportsa nportsb) "Matrix must be square")
    (mat/matrix (for [i (range nfreqs)
                      :let [Y (mat/squeeze (mat/idx data i :all :all))]]
                  (let [G (mat/diag z0)
                        F (mat/diag (-> (cmplx/real z0)
                                        abs
                                        sqrt
                                        mat/dot-divide
                                        (* 0.5)))]
                    (* F (- (mat/eye nportsa) (* (cmplx/conjugate G) Y)) (mat/inv (+ (mat/eye nportsa) (* G Y))) (mat/inv F)))))))

; 2-port parameters
(defmethod to-s :abcd [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)
        z01 (mat/idx z0 0)
        z02 (mat/idx z0 1)]
    (assert (= nportsa nportsb 2) "ABCD parameters must have two ports")
    (mat/matrix (for [i (range nfreqs)]
                  (let [A (mat/idx data i 0 0)
                        B (mat/idx data i 0 1)
                        C (mat/idx data i 1 0)
                        D (mat/idx data i 1 1)
                        denom (+ (* A z02) B (* C z01 z02) (* D z01))]
                    [[(/ (+ (* A z02) B (* -1 C (cmplx/conjugate z01) z02) (* -1 D (cmplx/conjugate z01)))
                         denom) ;S11
                      (/ (* 2 (- (* A D) (* B C)) (sqrt (* (cmplx/real z01) (cmplx/real z02))))
                         denom)] ;S12
                     [(/ (* 2 (sqrt (* (cmplx/real z01) (cmplx/real z02))))
                         denom) ;S21
                      (/ (+ (* (- A) (cmplx/conjugate z02)) B (* -1 C z01 (cmplx/conjugate z02)) (* D z01))
                         denom)]]))))) ;S22

(defmethod to-s :h [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)
        z01 (mat/idx z0 0)
        z02 (mat/idx z0 1)]
    (assert (= nportsa nportsb 2) "H parameters must have two ports")
    (mat/matrix (for [i (range nfreqs)]
                  (let [h11 (mat/idx data i 0 0)
                        h12 (mat/idx data i 0 1)
                        h21 (mat/idx data i 1 0)
                        h22 (mat/idx data i 1 1)
                        denom (- (* (+ z01 h11) (+ 1 (* h22 z02))) (* h12 h21 z02))]
                    [[(/ (- (* (- h11 (cmplx/conjugate z01)) (+ 1 (* h22 z02))) (* h12 h21 z02))
                         denom)
                      (/ (* 2 h12 (sqrt (* (cmplx/real z01) (cmplx/real z02))))
                         denom)]
                     [(/ (* -2 h21 (sqrt (* (cmplx/real z01) (cmplx/real z02))))
                         denom)
                      (/ (+ (* (+ z01 h11) (- 1 (* h22 (cmplx/conjugate z02)))) (* h12 h21 (cmplx/conjugate z02)))
                         denom)]])))))

(defmethod to-s :t [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)]
    (assert (= nportsa nportsb) "Matrix must be square")
    (mat/matrix (for [i (range nfreqs)
                      :let [[T11 T12 T21 T22] (internal-external-partition (mat/squeeze (mat/idx data i :all :all)))]]
                  (mat/matrix [[(* T12 (mat/inv T22)) (- T11 (* T12 (mat/inv T22) T21))]
                               [(mat/inv T22) (* -1 (mat/inv T22) T21)]])))))

(defmulti from-s :to)

; N-port parameters
(defmethod from-s :s [{:keys [data z0]}] data)

(defmethod from-s :z [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)]
    (assert (= nportsa nportsb) "Matrix must be square")
    (mat/matrix (for [i (range nfreqs)
                      :let [S (mat/squeeze (mat/idx data i :all :all))]]
                  (let [G (mat/diag z0)
                        F (mat/diag (-> (cmplx/real z0)
                                        abs
                                        sqrt
                                        (* 0.5)))]
                    (* (mat/inv F) (mat/inv (- (mat/eye nportsa) S)) (+ (* S G) (mat/ctranspose G)) F))))))

(defmethod from-s :y [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)]
    (assert (= nportsa nportsb) "Matrix must be square")
    (mat/matrix (for [i (range nfreqs)
                      :let [S (mat/squeeze (mat/idx data i :all :all))]]
                  (let [G (mat/diag z0)
                        F (mat/diag (-> (cmplx/real z0)
                                        abs
                                        mat/dot-divide
                                        (* 0.5)))]
                    (* (mat/inv F) (mat/inv (+ (* S G) (cmplx/conjugate G))) (- (mat/eye nportsa) S) F))))))

; 2-port parameters
(defmethod from-s :abcd [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)
        z01 (mat/idx z0 0)
        z02 (mat/idx z0 1)]
    (assert (= nportsa nportsb 2) "ABCD parameters must have two ports")
    (mat/matrix (for [i (range nfreqs)]
                  (let [S11 (mat/idx data i 0 0)
                        S12 (mat/idx data i 0 1)
                        S21 (mat/idx data i 1 0)
                        S22 (mat/idx data i 1 1)
                        denom  (* 2 S21 (sqrt (* (cmplx/real z01) (cmplx/real z02))))]
                    [[(/ (+ (* (+ (cmplx/conjugate z01) (* S11 z01)) (- 1 S22)) (* S12 S21 z01)) denom)
                      (/ (- (* (+ (cmplx/conjugate z01) (* S11 z01)) (+ (cmplx/conjugate z02) (* S22 z02)))
                            (* S12 S21 z01 z02)) denom)]
                     [(/ (- (* (- 1 S11) (- 1 S22)) (* S21 S12)) denom)
                      (/ (+ (* (- 1 S11) (+ (cmplx/conjugate z02) (* S22 z02))) (* S12 S21 z02)) denom)]])))))

(defmethod from-s :h [{:keys [data z0]}]
  (let [[nfreqs nportsa nportsb] (mat/shape data)
        z0 (fix-z0-shape z0 nportsa)
        z01 (mat/idx z0 0)
        z02 (mat/idx z0 1)]
    (assert (= nportsa nportsb 2) "H parameters must have two ports")
    (mat/matrix (for [i (range nfreqs)]
                  (let [s11 (mat/idx data i 0 0)
                        s12 (mat/idx data i 0 1)
                        s21 (mat/idx data i 1 0)
                        s22 (mat/idx data i 1 1)
                        denom (+ (* (- 1 s11) (+ (cmplx/conjugate z02) (* s22 z02))) (* s12 s21 z02))]
                    [[(/ (- (* (+ (cmplx/conjugate z01) (* s11 z01)) (+ (cmplx/conjugate z02) (* s22 z02))) (* s12 s21 z01 z02))
                         denom)
                      (/ (* 2 s12 (sqrt (* (cmplx/real z01) (cmplx/real z02))))
                         denom)]
                     [(/ (* -2 s21 (sqrt (* (cmplx/real z01) (cmplx/real z02))))
                         denom)
                      (/ (- (* (- 1 s11) (- 1 s22)) (* s12 s21))
                         denom)]])))))

(defmethod from-s :t [{:keys [data z0]}] ;https://scikit-rf.readthedocs.io/en/latest/_modules/skrf/network.html#s2t
  (let [[nfreqs nportsa nportsb] (mat/shape data)]
    (assert (= nportsa nportsb) "Matrix must be square")
    (mat/matrix (for [i (range nfreqs)
                      :let [[S11 S12 S21 S22] (internal-external-partition (mat/squeeze (mat/idx data i :all :all)))]]
                       ; Note Sii are matrices in the case of a n > 2 even s matrix being input
                  (mat/matrix [[(- S12 (* S11 (mat/inv S21) S22))
                                (* S11 (mat/inv S21))]
                               [(* (mat/inv (- S21)) S22)
                                (mat/inv S21)]])))))

(defn convert [input]
  (if (= (:from input) (:to input))
    (:data input)
    (if (= (:to input) :s)
      (to-s input)
      (from-s (assoc input :data (to-s input))))))

(defn renormalize
  "Takes `s` an NxMxM matrix of s-parameters referenced to 
   `z0-current`, and returns a NxMxM matrix of s-parameters
   referenced to `z0-desired`."
  [s z0-current z0-desired]
  (to-s {:from :z :data (from-s {:to :z :data s :z0 z0-current}) :z0 z0-desired}))

(defn passivity [])

(defn reciprocity [s]
  (mat/matrix (for [i (range (first (mat/shape s)))
                    :let [s (mat/squeeze (mat/idx s i :all :all))]]
                (- s (mat/transpose s)))))

(defn passive? [])

(defn active? [s])

(defn reciprocal? [])
