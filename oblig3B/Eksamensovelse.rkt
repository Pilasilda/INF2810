'--Exercise1.4--
(define (a-plus-abs-b a b)
  ((if (> b 0 ) + - ) a b)) 


(a-plus-abs-b 9 0)

(a-plus-abs-b  -1 4)

;Sjekker forst om 9 er storre enn 0, og i dette tilfellet er det det derfor tar den b+0 som da er 9.
;Sjekker forst om (-1) > 3 og i dette tilfellet er det ikke det. Derfor tar gaar if sjekke til aa evaluere - i dette uttrykket.


'--Exercise1.5--
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;(test 0 (p))

;Det forste man kan se her er at (p) (p) blir x og y i prosedyren definert under. Saa som i kallet over saa blir x her 0,
;derfor vil ikke dette uttrykket gaa lengere enn at den returnerer 0 fordi x=0. Det er ogsaa derfor ikke nodvenidg aa vite hva
;y er i dette tilfellet



'--Exercise1.6--
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else-clause)))

(new-if (= 2 3) 0 5)
;Det som skjer her er at hvis 2=3 returneres 0, hvis ikke 5. 2 er ikke lik 3 og derfor returneres 5

(new-if (= 1 1) 0 5)
;Det 

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

;Det som skjer her er at hvis 1=1 og derfor vil det returneres 0 fordi det er
;returverdien for positivt svar. new-if er en egen prosedyre og vil derfor evaulere sin egen prosedyre
;uavhengig av good-enough vil returnere sant eller usant


'--Exercise1.9--
;(define (+ a b)
 ; (if (= a 0)
  ;    b
   ;   (inc (+ (dec a) b))))

;Denne gir opphav til rekursiv prosess 

;(define (+ a b)
 ; (if (= a 0)
  ;    b
   ;   (+ (dec a) (inc b))))

;Denne gir oppgav til iterativ prosess

(+ 4 5)

'--Exercise2.17--
(define (last-pair list)
  (if (null? (cdr list)) ; hvis listen er null saa cdr av listen som er siste element av liste
      list ; returnere listen 
      (last-pair (cdr list)))) ;gaar gjennom alle elementer i listen og returerer til slutt det
;siste elementet av listen


(last-pair (list 23 72 149 34))


'--Exercise2.18--
(define (reversere list)
  (if (null?  list)
      list
      (append (reverse (cdr list)) (car list))))

(reversere (list 1 4 9 16 25))


'--Exercise2.21--
(define (square x)
  (* x x))


(define (square-list items)
  (if (null? items)
      nil
      (cons (sqaure (car items))
            (square-list (cdr items)))))

(define (square-list items)
  (map square items))
;Lagrer en mappe av alle t

(square-list (list 1 2 3 4))
;Tar kvadratrot av tallene i listen

'--Exercise2.22--
#| 
(define (square-list items)
  (define (iter things answer)
  (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items nil))|# 


#|(define (square-list items)
  (define (iter things answer)
  (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items nil)) |# 



(square-list (list 1 2 3 4))
;(iter (1 2 3 4) ())
;(iter (2 3 4) (() . 1))
;(iter (3 4) ((() . 1) . 4))
;(iter (4) (((() . 1) . 4) . 9))
;(iter () ((((() . 1) . 4) . 9) . 16))
;((((() . 1) . 4) . 9) . 16) 

'--Exercise2.23--
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

(newline)
 


'--Exercise2.53--
(list'a 'b 'c) ; -> a b c
(list (list 'george)) ; -> ((george))
(cdr '((x1 x2) (y1 y2))) ; -> ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; -> (y1 y2) 
(pair? (car '(a short list))) ; -> #f 
;(meq 'red '((red shoes) (blue socks))) ; -> #f 
;(memq 'red '((red shoes) (blue socks))) ; -> (red shoes blue socks)


'--Exercise2.54--
#|
(define (equal? a b)
  (or (eq? a b)
      (and (pair? a) (pair? b)
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))|#


'--Exercise2.55--
(car ''abracadabra)
; ('') er kun en syntaktisk sukker, uttrykk '(1 2 3) (1 2 3) er de samme, men i dette tilfellet
; (quote (quote abracadabra)) dette igjen evalueres til (quote adacadabra), car i denne listen er
; quote og derfor skrives quote ut.



'--Exercise1.30--
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


;Iterativ prosedyre 
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result 
        (iter (next a) (+ result (term a)))))
     (iter a 0))
      


'--Exercise1.31--

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define foo '(a b))
(define bar foo)
(set! foo '(c d))
(set-car! foo bar)
(set-car! (cdr foo) (car foo))
(set-cdr! bar (list 7)) ;grunnen til a7 a7 er fordi at kun verdien i cdr av foo endres, saa alle cdr verdier endres til 7
foo



((lambda (x) (* x x)) 4)




(define (reduce proc init items)
  (if (null? items)
      init
      (proc (car items)
            (reduce proc init (cdr items)))))

(reduce cons '() '(1 2 3 4 5))



'--Oppgave1--
(car '(1 2))
(car (cons 2 3))

(append '(1 2) '(3 4))
(append '(1 2) '(3 (4 5) '(2)))
(append)


(define (test list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (cdr list2))))


(test '(1 2 3) '(4 5 6))



(define (test1 list1 list2)
  (if (null? list1)
      list2 
      (append (list (car list1) (cdr list2)))))

(test1 ' (1 2 3 4 5) '(6 7 8 9))



(define (anti-sum lst)
  (apply - 1 lst))


;(anti-sum '(1 2 3))



((lambda (x y) (+ x y)) 3 4)

(define test
  (lambda (x y)
    (- x y)))

(test 3 4)
(test 4 2)


'--Oppgave1-- 
;(define (test1 list1 list2)
 ; (if (null? list1)
  ;    list2
   ;   (apply list1 (cdr list2))))


;(test1 '(list1 1 2) '(list2 3 4 ))


;(apply + '(2 3 4))
;(apply - '(2 3 4))


'--Oppgave2Strommer--
;Definerer car av stream som er hodet 
(define (stream-car stream)
  (car stream))

;Definerer cdr av stream som er halen
(define(stream-cdr stream)
  (cdr stream))

;Definerer den tomme listen som er hvis strommen er tom
(define (empty-streams)'())

;Definerer delay som utsetter utskrift av verdi
(define (delay x)
  (lambda() x)) ;hentes kun x ikke y

;Definerer cons-stream som utsetter y-verdien som i dette tilfellet er cdr av listen
(define (cons-stream x y)
  (cons x (delay y)))

;Konser halen med hodet
(define (add-streams s1 s2)
  (cons-stream (+ (stream-car s1) (stream-cdr s2))
               (add-streams (stream-car s1)
                            (stream-cdr s2))))


;Kaller cons-stream som utsetter cdr av listen, og skriver kun ut car av listen
(cons-stream (+ 1 2)
             (+ 3 4))

;Kaller stream-cdr for aa kunne skrive ut, cdr verdi av listen
(stream-cdr (cons-stream (+ 1 2)(+ 3 4)))


'--Oppgave3Pair?append/cons/apply--
;Pair? 
(define (test x)
  (pair? x))
;Kall for test
(test '(1))

(pair? 1) ;Gir #f fordi 1 ikker har et tall som paret

(pair? '(1 2 3)) ;gir #t fordi 1 2 3 er et par

(pair? '()) ;gir #false fordi '()/tomliste og bestaar av ingen elementer 

;Append
(define test '(1 2 3))
(define test1 '(4 5 6))
;appender test og test1 -> (1 2 3 4 5 6)
(append test test1)


;Cons
(define (test2 list)
  (cons (cdr list) (car list)))

;(test2 '(2 3 1))

;Forskjellen paa cons og append er at append gir en liste med tallene hvis du slaar sammne to lister, mens cons slaar ogsaa sammen
;men cons setter det som ((1 2 3) 4 5 6) 
(define test '(1 2 3))
(define test1 '(4 5 6))

;(cons test test1)

;Apply
(apply + 1 2 3 (list 4 5 6))

(define (test f . args)
  (apply f args))

(test + 1 2 3)
(test * 2 5)
(test - 4 2)
(test - 2 3 4 5)


'--Oppgave4--
#|
(define (map proc list)
  (if (null? list)
      '()
      (cons (proc (car list))
            (map proc (cdr list))))) |# 

;(map pair? '(3 4 5 6))


'--Oppgave5--

(define (list-copy items)
  (if (null? items)
      '()
      (cons (car items)
            (list-copy (cdr items)))))

#|provde med if, men hvis du sender med (1) ved 
(define (every-other items)
  (if (null? items)
      '()
      (cons (car items)
            (every-other (cdr (cdr items))))))|# 

(define (every-other items)
  (cond ((null? items) '())
        ((null? (cdr items)) items)
        (else (cons (car items)
                    (every-other (cdr (cdr items)))))))

;Kall for every-other
(every-other '(1 2 3 4))

(every-other '(1))

(every-other '(1 2 3 4 5))

(every-other '())

'--Oppgave6--
;Increase  
(define (increase-by n items)
  (if (null? items)
      '()
      (cons (+ (car items) n)
            (increase-by n (cdr items)))))
;Kall paa increase
(increase-by 2 '(1 2 3))

;Increase  
(define (increase-by2 n items)
  (map (lambda (x) (+ n x)) items))

;kall paa increase
(increase-by2 2 '(2 2 3))

;Make-increase 
(define (make-increaser n)
  (lambda (x) (+ n x)))

;increase kaller paa make-increase som er prosedyre som lager en lambdauttrykk som adderer
;n med x
(define (increase-by3 n items)
  (map (make-increaser n) items))

;Kall 
(increase-by3 3 '(3 4 5))



(define foo 'boff)
(define bar 'baff)

(define (baz foo) ;definerer bazz som sender med foo
  (let ((bar 'bing)) ; bar er bing 
    (list foo bar))) ; foo

(baz bar)

(baz 'bang) ; -> (bang, bing)

(let ((foo 42) ;foo er 42
      (bar foo)) ;bar 
  (list bar foo)) ; -> (baff, 42)

(let ((foo (lambda (bar) (list bar foo))) 
      (bar foo)) 
  (list (foo bar))) 



;(lambda(baff) (baff, boff)

;Let
(let ((foo (list 1 2))
      (bar (* 3 4)))
  (cons bar foo))

;Lambda 
((lambda (foo bar)
  (cons bar foo))
 (list 1 2)(* 3 4))

;Let 
(let ((foo (list 1 2)))
  (display foo)
  (newline)
  (let ((foo (cons 0 (cdr foo))))
    (display foo)))

;Lambda
(lambda (foo)
  (display foo)
  (newline)
  (lambda (foo)
     (display foo)
    (cons 0 (cdr foo)))
 (list 1 2))


'--Oppgave7--
;Det lages en prosedyre foo med argumentene x y z, som vil si at argumentene er selvalgt hva de skal representere
;prosedyren returnerer en liste av elementer hvor y er anvendt paa x. Som innebaerer at
;x er her kan f.eks vaere et predikat.


'--Oppgave8--
(define (compose x y)
  (lambda (z) (x (y z))))

'--Oppgave9--
(define foo '(2 3 3 4)) ;(2 3 3 4)
(define bar (cons (car foo) (cdr foo))) ;(2 3 3 4)
(set-car! foo 1)
(set-car! (cdr foo) 2)
(set! bar (cons 0 bar))
bar
foo

#|
(define (x y z)
  (cond ((null? z) '())
        ((y (car z))
         (cons (car z)
               (x y (cdr z))))
        (else (x y (cdr z)))))|#



'--Oppgave10--
#| 
(define (take n l)
  (if (or (null? l)
         ((zero? n))
         '()
         (cons (car 1)
               (take (- n 1) (cdr 1))))))|# 


(define foo '(a b c d e))
;(take 3 foo) 


(define (count n)
  (cond ((zero? n) 'done)
        (else (count (- n 1))))(display n))
(count 2)


'--Oppgave11--
(define foo (list "bring" "towels" "!")) ; (bring towels!)
(set-car! foo foo) ;(bring towels! towels!)
(set-cdr! (cdr foo) 42) ; (bring towels 42)

(define foo '(a b))
(define bar foo)
(set! foo '(c d))
(set-car! foo bar) ;foo bindes til bar
(set-car! (cdr foo) (car foo)) ;cdr av foo er (d '()) som settes til car av foo som er (a b), derfor blir foo ((a b) (a b))
(set-cdr! bar (list 7)) ; endrer kun verdien av bar i for alle cdr verdien


(define foo '(2 3 3 4))
(define bar (cons (car foo) (cdr foo)))
(set-car! foo 1)
bar
(set-car! (cdr foo) 2)
(set! bar (cons 0 bar))
(cdr bar)


'--Oppgave12--
#| 
(define (deep-map proc nested)
  (if (null? nested)
      '()
      (cons (deep-map proc (car nested)
            (deep-map proc (cdr nested))))))|# 

;(define nested '(((1) 2) 3 (4 (5 6))))
;(deep-map (lambda (x) (* x 10)) nested)

;en rekursiv prosess 

'--Oppgave13--
;Vanlig rekursjon
#| (define (replace x y seq)
  (if(null? seq)
     '()
     (cons (if (eq? x (car seq)
                    (replace x y(cdr seq))))))) |# 



(define z1
  (let ((foo (list 'a 'b)))
    (cons foo foo)))

(define z2
  (cons (list 'a 'b) (list 'a 'b)))



'--Oppgave2Mysterium--
#|Definerer en prosedyre x med predikatet y og listen z, og sjekker om listen er null returneres true
hvis ikke saa maa alle elementene sjekkes opp mot predikatet. 
|#

(define (x y z)
  (or (null? z)
      (and (y (car z))
           (x y (cdr z)))))

(x even? '(4 8))
(x odd? '(3 2))

'---3Rekursjon
(define (avg first . rest)
 (define (iter numbers sum count )
   (if (null? numbers)
       (/ sum count)
       (iter (cdr numbers) (+ (car numbers) sum) (+ 1 count))))
   (iter (cons first rest) 0 0))

(avg 1 2 3)

'--3Transform-if
#|(define (transform-if test trans seq)
  (if (null? seq)
     '()
     (cons (cond (test (car seq))
                 (trans(cdr seq))
                 (car seq))
           (transfomr-if test trans (cdr seq)))))

 (transform-if odd? (lambda (x) (+ x 1)) foo)|#

(define (trans-if pred proc items)
  (define (iter item)
    (cond ((null? item) items)
          ((pred (car items))
           (begin (set! (car item) (proc (car item)))
                  (iter pred proc (cdr item))))
                 (else(iter pred proc (cdr item)))))
    (iter items))

(define foo '(1 2 3 4))
(transform-if odd? (lambda (x) (+ x 1)) foo)