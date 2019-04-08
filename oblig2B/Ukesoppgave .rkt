'--Exercise-1.4--

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 4 5)

;1) er 5(b) storre enn 4, ja
;2) hvis b er positiv saa skal a og b adderes
;3) hvis b er negativ saa skal det subtraheres


'--Exercise-1.5--

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))



'--Execise-1.6--
(define (new-if predicate then-clause els-clause)
  (cond (predicate then-clause)
        (else else-clause)))