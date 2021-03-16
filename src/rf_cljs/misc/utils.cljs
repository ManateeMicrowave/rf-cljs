(ns rf-cljs.math.utils)

(defn linear [db]
  (Math/pow 10 (/ db 10)))

(def prefix
  "Scaling values for metric prefixes"
  {:f 1e-15
   :p 1e-12
   :n 1e-9
   :u 1e-6
   :m 1e-3
   :base 1e0
   :k 1e3
   :M 1e6
   :G 1e9
   :T 1e12
   :P 1e15})

(defn as-prefix
  "Given a `value`, returns the closest metric prefixed number as a `[value :prefix]` pair"
  [value]
  (letfn [(pf-scale [value pf]
            [(/ value (prefix pf)) pf])]
    (cond
      (< value (prefix :p))    (pf-scale value :f)
      (< value (prefix :n))    (pf-scale value :p)
      (< value (prefix :u))    (pf-scale value :n)
      (< value (prefix :m))    (pf-scale value :u)
      (< value (prefix :base)) (pf-scale value :m)
      (< value (prefix :k))    (pf-scale value :base)
      (< value (prefix :M))    (pf-scale value :k)
      (< value (prefix :G))    (pf-scale value :M)
      (< value (prefix :T))    (pf-scale value :G)
      (< value (prefix :P))    (pf-scale value :T)
      :else                    (pf-scale value :P))))

(defn as-value
  "Given a `[value :prefix]` pair, return the true value"
  [[value pf & _]]
  (* value (prefix pf)))
