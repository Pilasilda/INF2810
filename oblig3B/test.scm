;Oblig1 

;Oppgave 2 Kontrollstruktur og egendefinerte prosedyrer
(define (sign tall)
  (cond ((< tall 0)-1)
        ((> tall 1)1)
       (else (= tall 0) 0)))        
;(sign 5)


(define (sign1 tall)
  (or
   (and(< tall 0)-1)
   (and(> tall 1) 1)
       (and(= tall 0)0)))
;(sign1 3)


;Oppgave 3 rekursjon,iterasjon og blokkstruktur
;Oppgave3A 
(define (add1 x)
  (+ x 1))

(define (sub1 x)
  (- x 1))

(add1 3)
(sub1 2)
(add1 (sub1 0))

;Oppgave3B
;Rekursiv
(define (plus x y)
  (add1 (plus (sub1 x) y)))

;Iterativ
(define (minus x y)
  (if(zero? x)
     y
     (plus (add1 x) (sub1 y))))


;Oppgave3D

(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
  (power-iter 1))

;Oppgave3E
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fib 8)

;Oblig2

(define test '(0 42 #t bar))

;(car (cdr test))

(define test1 '((0 42)(#t bar)))

(car (cdr (car test1)))

(define test2 '((0) (42 #t) (bar)))

;(car (car (cdr test2)))

(list '(0 42) '(#t bar))

;Oppgave2 rekursjon over lister og hoyereordens prosedyrer
'--Oppgave2A 
(define (length2 items)
  (define (iter liste count)
    (if(null? liste)
       count
       (iter (cdr liste) (+ count 1))))
  (iter items 0))

(length2 '(1 2 3 4 5 6 7 8))

'--Oppgave2B
(define (reduce-reverse proc init items)
  (define (iter liste count)
    (if (null? liste)
        count
        (iter (cdr liste)
              (proc (car liste) count))))
  (iter items init))


(reduce-reverse cons '() '(1 2 3 4))

'--Oppgave2C
(define (all? pred seq)
  (or (null? seq)
      (and (pred(car seq))
           (all? pred (cdr seq)))))

(all? (lambda (x) (* x 10)) '(2 3))
(all? odd? '())

'--Oppgave2D
(define (nth tall liste)
    (if(zero? tall)
       (car liste)
       (nth (- tall 1)(cdr liste))))

(nth 2 '(34 5 65 5 4))


'--Oppgave2E
#|
(define (where tall seq)
  (define (iter liste count)
    (if (null? liste)
        0
         (or (= tall (car liste) count)
            (iter (cdr liste) (+ count 1))))))
  (iter seq 0))
        

(where 3 '(1 2 3 4 5 6 6 7 ))|# 

'--Oppgave2F
(define (map2 proc liste1 liste2)
  (if (or (null? liste1)
          (null? liste2))
      '()
      (cons (proc (car liste1)
                  (car liste2))
            (map2 proc
                  (cdr liste1)(cdr liste2)))))
   
(map2 + '(1 2 3 4) '(3 4 5))

'--Oppgave2G
(map2 (lambda (x y)
         (/ (+ x y) 2))
        '(1 2 3 4) '(3 4 5))

'--Oppgave2H
(define (both? pred)
  (lambda (x y)
    (and (pred x)
         (pred y))))

(map2 (both? even?) '(1 2 3) '(3 4 5))
((both? even?) 2 4)
((both? even?) 2 5)

'--Oppgave2I
(define (self proc)
  (lambda(x) (proc x x)))

((self +) 5)
((self *) 3)
(self +)
((self list) "hello")








