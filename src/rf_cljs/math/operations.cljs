(ns rf-cljs.math.operations
  (:refer-clojure :exclude [* / + - fix mod < > <= >=])
  (:require
   ["mathjs" :as mathjs]))

(def default-eps 1e-10)

(defn *
  ([] 1)
  ([x] x)
  ([x y & rest] (apply mathjs/multiply x y rest)))

(defn /
  ([x] (mathjs/divide 1 x))
  ([x y & rest] (mathjs/divide x (apply * y rest))))

(defn +
  ([] 0)
  ([x] (mathjs/unaryPlus x))
  ([x y & rest] (apply mathjs/add x y rest)))

(defn -
  ([x] (mathjs/unaryMinus x))
  ([x y & rest] (mathjs/subtract x (apply + y rest))))

(defn <
  ([x] true)
  ([x y] (mathjs/smaller x y))
  ([x y & rest] (if (mathjs/smaller x y)
                  (if (next rest)
                    (recur y (first rest) (next rest))
                    (mathjs/smaller y (first rest)))
                  false)))

(defn <=
  ([x] true)
  ([x y] (mathjs/smallerEq x y))
  ([x y & rest] (if (mathjs/smallerEq x y)
                  (if (next rest)
                    (recur y (first rest) (next rest))
                    (mathjs/smallerEq y (first rest)))
                  false)))

(defn >
  ([x] true)
  ([x y] (mathjs/larger x y))
  ([x y & rest]
   (if (mathjs/larger x y)
     (if (next rest)
       (recur y (first rest) (next rest))
       (mathjs/larger y (first rest)))
     false)))

(defn >=
  ([x] true)
  ([x y] (mathjs/largerEq x y))
  ([x y & rest]
   (if (mathjs/largerEq x y)
     (if (next rest)
       (recur y (first rest) (next rest))
       (mathjs/largerEq y (first rest)))
     false)))

(defn abs [x]
  (mathjs/abs x))

(defn cbrt [x]
  (mathjs/cbrt x))

(defn ceil [x]
  (mathjs/ceil x))

(defn cube [x]
  (mathjs/cube x))

(defn exp [x]
  (mathjs/exp x))

(defn expm1 [x]
  (mathjs/expm1 x))

(defn fix [x]
  (mathjs/fix x))

(defn floor [x]
  (mathjs/floor x))

(defn gcd [a b]
  (mathjs/gcd a b))

(defn hypot [a b & rest]
  (apply mathjs/hypot a b rest))

(defn lcm [a b]
  (mathjs/lcm a b))

(defn log [x & base]
  (apply mathjs/log x base))

(defn log10 [x]
  (mathjs/log10 x))

(defn log1p [x]
  (mathjs/log1p x))

(defn log2 [x]
  (mathjs/log2 x))

(defn mod [x y]
  (mathjs/mod x y))

(defn norm [x & p]
  (apply mathjs/norm x p))

(defn nth-root [a]
  (mathjs/nthRoot a))

(defn nth-roots [x]
  (mathjs/nthRoots x))

(defn pow [x y]
  (mathjs/pow x y))

(defn round [x & n]
  (apply mathjs/round x n))

(defn sign [x]
  (mathjs/sign x))

(defn sqrt [x]
  (mathjs/sqrt x))

(defn square [x]
  (mathjs/square x))

(defn xgcd [a b]
  (mathjs/xgcd a b))

(defn acos [x]
  (mathjs/acos x))

(defn acosh [x]
  (mathjs/acosh x))

(defn acot [x]
  (mathjs/acot x))

(defn acoth [x]
  (mathjs/acoth x))

(defn acsc [x]
  (mathjs/acsc x))

(defn acsch [x]
  (mathjs/acsch x))

(defn asec [x]
  (mathjs/asec x))

(defn asech [x]
  (mathjs/asech x))

(defn asin [x]
  (mathjs/asin x))

(defn asinh [x]
  (mathjs/asinh x))

(defn atan [x]
  (mathjs/atan x))

(defn atan2 [x y]
  (mathjs/atan2 x y))

(defn atanh [x]
  (mathjs/atanh x))

(defn cos [x]
  (mathjs/cos x))

(defn cosh [x]
  (mathjs/cosh x))

(defn cot [x]
  (mathjs/cot x))

(defn coth [x]
  (mathjs/coth x))

(defn csc [x]
  (mathjs/csc x))

(defn csch [x]
  (mathjs/csch x))

(defn sec [x]
  (mathjs/sec x))

(defn sech [x]
  (mathjs/sech x))

(defn sin [x]
  (mathjs/sin x))

(defn sinh [x]
  (mathjs/sinh x))

(defn tan [x]
  (mathjs/tan x))

(defn tanh [x]
  (mathjs/tanh x))

(defn approx
  "Determines if floats `x` and `y` are within `eps` of one another"
  ([x y] (approx x y default-eps))
  ([x y eps]
   (< (abs (- x y)) eps)))

(defn lt-approx
  ([x y] (lt-approx x y default-eps))
  ([x y eps]
   (or (approx x y eps) (< x y))))

(defn gt-approx
  ([x y] (gt-approx x y default-eps))
  ([x y eps]
   (or (approx x y eps) (> x y))))
