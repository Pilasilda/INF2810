(define (transform-if test trans seq)
  (if (null? seq)
      '()
      (cons (if (test (car seq))
                 (trans (car seq))
                 (car seq))
      (transform-if test trans (cdr seq)))))

(define foo '(1 2 3 4 5 6))
  
 (transform-if odd? (lambda (x) (+ x 1)) foo)



(define (test seq)
  (define (iter lst tall)
    (if (null? lst)
        tall
        (iter (cdr lst) (+ tall 1))))
  (iter seq 0))

(test '(1 2 3 4))




#|(define (transform-if! test trans seq)
  (define (iter lst lst2)
    (cond ((null? lst)lst2)
          ((test (car lst))
    (iter (cdr lst) (append lst (trans (car lst)))
          (else (iter (cdr lst) (append lst2 (car lst)))
                       (iter seq '())|#


(cons '(1 2) '(2 3))



(define (avg first . rest)
  (define (iter count sum list)
    (if (null? count)
        (/sum count) 
        (iter (cdr list)(+ sum (cdr items) (+ 1 count)))))
  (iter (cons first rest) 0 0))





#|(define (avg first . rest count item)
  (if(null? item)
     0
    (cons (avg (cdr item) (+ sum (cdr item) (+ 1 count))))))|#




'--oppgave1 
(define (avg first . rest)
  (let ((count 0))
    (define (iter item)
                  (if (null? item)
                      0
                      (begin (set! count (+ 1  count)) (+ (car item) (iter (cdr item))))))
    (/ (iter (cons first rest)) count)))

  (avg 1 2 3)