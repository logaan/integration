(remove-ns 'integration.core)
(ns integration.core
  (:require [matchure :refer [defn-match]]
            [clojure.test :refer :all]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(comment

Examples
- Should there be a distinction between functions like sin and unbound
  variables like x? I suppose division and multiplication are functions too.
- Maybe div, mult, sin, etc should each be a record. If we don't need to delve
  deep into the arguments to the expressions then we might be able to do
  integration purely through a protocol rather than pattern matching.
- Calculus.pdf mentions a selection of integration formulas are inside the
  cover of the book.
- If you don't have an integration forumula or rule for integration then you
  can still use numerical integration.

  )

; v = cos x
; f = sin x
(def v [:cos :x])
(def f [:sin :x])

; v = x
; f = 1/2xpow2
(def v :x)
(def f [:div [:pow :x 2] 2])
; Or maybe it's better expressed as
(def f [:mult 1/2 [:pow :x 2]])

(comment

 v = velocity
 f = distance
 
 v1,v2,v3,v4 = 1,2,3,4
 f1,f2,f3,f4 = 1,3,6,10
 
 to go from fs to vs diff each f with previous
 to go from vs to fs sum vs
 
 (+ v1 v2 ... vn) = (fn - f0)
 eg. (+ 1 2 3 4) = (10 - 0)
 eg. (+ 1 3 5 ... 99)
   = (+ 1 4 9 16 ... 2500)
   = (+ (pow 1 2) (pow 2 2) (pow 3 2) (pow 4 2) ... (pow 50 2))
   = (- (pow 50 2) (pow 0 2))
   = (- 2500 0)
 
 So you find the function (f x) then plug in the start and end of the range
 (- (f n) (f 0)) to find the sum of vs in that range (+ v1 v2 v2... vn)
 
 When you move from descrete values to limits you can no longer use algebra to
 reach the solution. You need to switch to calculus. It's like switching from x
 being integers (1,2,3,...) to x being real numbers (1.0,1.1,1.2,etc) but you
 can drop down to infinite precision (1.000000,1.000001,etc).
 
 The exercises in 5.2 begin to be useful.

Chapter PDF page 192 - 197 Solutions on PDF page 644.
 
 (def exercise-5.2.1
   (+ (* 5 (pow x 4)) (* 4 (pow x 5))))

 ;x^5+2/3x^6;5/3
 (+ (pow x 5) (* 2/3 (pow x 6)))


  )

; These may produce errors at some point but for now they keep things as
; rationals
(defn pow [x y] (int (Math/pow x y)))
(defn sqrt [x] (int (Math/sqrt x)))

(deftest imported-math
  (is (= 4 (pow 2 2)))
  (is (= 2 (sqrt 4))))

; All this code would probably be much nicer if we weren't using symbols and
; lists.
(defn-match integrate
  ([(and ?unbound clojure.lang.Symbol)]
   `(* 1/2 (pow ~unbound 2)))
  ([['* (and ?multiplier java.lang.Long) (and ?unbound clojure.lang.Symbol)]]
   `(* (/ ~multiplier 2) (pow ~unbound 2)))
  ([['* (and ?multiplier java.lang.Long)
     ['pow (and ?unbound clojure.lang.Symbol)
      (and ?exponent java.lang.Long)]]]
   `(* (/ ~multiplier (+ ~exponent 1)) (pow ~unbound (+ ~exponent 1))))
  ([['+ ?exp1 ?exp2]]
   `(+ ~(integrate exp1) ~(integrate exp2)))
  )

; So that we can use ` for the quoting rather than spelling out clojure.core/*.
; Use ~x to get 'x (rather than 'integration.core/x)
(def x 'x)

(deftest integrates
  (is (= `(* 1/2 (pow ~x 2)) (integrate x)))
  (is (= `(* (/ 4 2) (pow ~x 2)) (integrate '(* 4 x))))
  (is (= `(* (/ 4 (+ 2 1)) (pow ~x (+ 2 1))) (integrate '(* 4 (pow x 2)))))
  (is (= `(+ (* 1/2 (pow ~x 2)) (* 1/2 (pow ~x 2))) (integrate '(+ x x))))
  )

(def exercise-5-2-1
  (integrate '(+ (* 5 (pow x 4)) (* 4 (pow x 5)))))

(defn definite-integral [integral unbound up down]
  (let [lower (min up down)
        upper (max up down)]
    (- (eval `(let [~unbound ~upper] ~integral))
       (eval `(let [~unbound ~lower] ~integral)))))

(deftest does-definite-integral
  (is (= 5/3 (definite-integral exercise-5-2-1 'x 0 1)))
  (is (= 50N (definite-integral (integrate 'x) 'x 0 10))))

(run-tests)

