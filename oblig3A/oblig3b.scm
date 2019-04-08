(load "evaluator.scm")



(define (foo cond else)
  (cond ((= cond 2) 0)
        (else(else cond))))

(define cond 3)

(define (else x) (/ x 2))

(define (square x) (* x x))