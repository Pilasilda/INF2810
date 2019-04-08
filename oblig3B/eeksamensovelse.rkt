'--Oppgave1A 
(define z1
  (let ((foo (list 'a 'b)))
    (cons foo foo)))
z1

(define z2
  (cons (list 'a 'b) (list 'a 'b)))

z2


'--Oppgave1B
;Det som skjer her er at car verdien av z2 endres til aa vaere cdr verdien av z2 saa verdien av z2 vil vaere det sammen som z1
(set-car! z2 (cdr z2)) 


'--Oppgave1C
(define (nested-match x list)
  (define (iter count list2)
    (if (null? list2)
        '()
        (iter ((eq? x (car list2)) (+ 1 count)count))))
  (iter 0 list))

;(nested-match ’b ’((b) ((b a) b) a))



'--Oppgave2a
#|((lambda (foo bar)
   (cons foo bar))(list 1 2) (* 2 2)) |#

'--Oppgave2B
#| 
((lambda (foo)
   (display foo)
   (newline)
   ((lambda (foo)
      (display foo))
      (cons 0 (cdr foo)))) (list 1 2))|# 


'--Oppgave3A
(define (add1 x) (+ x 1))

(define (add100 x) (+ x 100))

(define (compose p1 p2)
  (lambda (x) (p1 (p2 x))))

((compose add1 add100) 5)


'--Oppgave3B
(define (repeat p n)
  (if(= n 1)
     p
  (compose p (repeat p (- n 1)))))


((repeat add1 10) 20) 

'--Oppgave3C
(define (eval-infix list)
  ((car(cdr list)) (car list) (car (cdr (cdr list)))))

(define exp1 (list 1 + 3))
(define exp2 (list 10 / 5))
(eval-infix exp1)
(eval-infix exp2)


'--Oppgave4A
#| 
(define (scale x seq)
  (define (iter count liste)
    (if (null? liste)
        '()
           (iter (cdr liste)
                 (cons (* (car liste) x) (+ 1 count)))))
  (iter 0 seq))|# 


;(define bar (list 1 2 3 5))
;(scale 3 bar)

'--Oppgave4B 
(define (scale2 x seq)
  (if (null? seq)
      '()
      (cons (* (car seq) x)
            (scale2 x (cdr seq)))))

(define foo (list 1 2 3 5))
(scale2 3 foo) 

'--Oppgave4D
(define (take n l)
    (if (or (null? l)
            (zero? n))
        '()
        (cons (car l)
              (take (- n 1) (cdr l)))))

(define foo '(a b c d e))
(take 3 foo)
(take 0 foo)
(take 7 foo)

; N forste elementer i listen
; hvis N > 1 sin length retur: l

'--OppgaveOmAverage
#|(define (avg first . rest)
  (define (iter count sum liste)
    (if (null? liste)
        (/ sum count)
        (iter (+ (sum (car liste) (+ count 1) (cdr liste))))))
  (iter first 1 rest))
(avg 1 2 3)|#
#|
(define (transform-if test trans seq)
  (if(null? seq)
     '()
     (cons (if (test (car seq))
              (trans (car seq)
           (transform-if trans (cdr seq)))))))
(define foo '(1 2 3 4))
(transform-if odd? (lambda(x) (+ x 1)) foo)|# 


'--Oppgave1A
#|
(define (list? x)
  (if (null? x)
      (and (pair? x)
          (list? (cdr x)))))|# 

'--Oppgave2D
#|
(define (deep-map proc nested)
    (if ((null? nested)
        '())
         (cons(deep-map proc (car nested)
              (deep-map proc (cdr nested))))))|# 

;(define nested '(((1) 2) 3 (4 (5 6))))
;(deep-map (lambda (x) (* x 10)) nested)



'--Oppgave2E
;siden det er nostede lister vi jobber med og med tanke paa hvordan den nestede listen er satt opp
;vil jeg tro det er rekursiv prosess dette kallet vil ta utgangspunkt i. Rekursive kall og prosesser
;bruker mer tid, fordi de maa vente paa svar fra forrige kall for aa kunne faa svar, alle kallene er avhengig
;av hverandre og det vil ta tid til vi faar tilbake svaret og minnet bruken er ogsaa stort.

'--Oppgave3A

(define (replace1 x y seq)
  (define (iter count list)
    (if(null? list)
       '()
       (iter (cdr list)      
             (cons (if (eq? x (car liste))
                       y
                       (car seq))list))))
  (iter seq '()))

(replace1 'a 'c '(a b b a))

'--Oppgave3B
(define (replace x y seq)
  (if (null? seq)
      '()
      (cons (if (eq? x (car seq))
                     y
                     (car seq))
                     (replace x y (cdr seq)))))
(replace 'a 'c '(a b b a))



- mer rekursjon og halerekursjon
- strommer, map
- Applicative order/ normal order



