(load "oblig2b_tester.scm")

'--1A--
(define(make-counter) ;lager en prosedyre make-counter
  (let((x 0)) ;lager er let uttrykk hvor x initialiseres til 0
  (lambda() 
    (set! x (+ x 1))x))) ;helt til dette er x bundet til 0 

;Kall fra oppgavetekst 
(define count 42)
(define c1 (make-counter)) 
(define c2(make-counter)) 
(c1) ;gir svar: 1
(c1) ;gir svar: 2
(c1) ;gir svar: 3
count ;gir svar: 42 
(c2) ;gir svar: 1


'--1B--
;Se vedlagt .pdf-fil 


'--2A--

(define(make-stack liste)
    (lambda (melding . args)
      (cond((eq? melding 'push!)
            (set! liste (append(reverse args) liste)))
           ((eq? melding 'pop!)
            (if (not (null? liste))
               (set! liste (cdr liste))
               liste))
           ((eq? melding 'stack) liste))))

;Kall fra oppgavetekst
(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack) ;gir bar
(s2 'pop!) ;tom stack
(s2 'push! 1 2 3 4) ; 4 3 2 1 
(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack)



'--2B--
(define(pop! stack-object)
  (stack-object 'pop!))

(define(push! stack-object . args)
  (apply stack-object 'push! args))

(define(stack stack-object)
  (stack-object 'stack))




(pop! s1)
(stack s1)
(push! s1 'foo ' faa)
(stack s1)


;Kall fra oppgavetekst for oppgave 3: 

(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(list-ref bar 0) 
(list-ref bar 3) 
(list-ref bar 4)
(list-ref bar 5)

'--3A--

;Se vedlagt .pdf-fil
; 1. liste-ref er en innebygd prosedyre som peker paa indeksen sin sitt symbol i listen.
; 2. (cdddr bar) peker til 'd, (cdr bar) peker tilbake til 'b, her lages det derfor en syklisklist fordi d vil alltid peke tilbake til b.
; 3. d sin indeks vil alltid vaere b. 

'--3B--
;Se vedlagt .pdf-fil

(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah 

;Aarsaken til at bah evaluerer til ((42 towel) 42) er fordi verdien car peker paa i bah settes til aa peke
;det samme objektet som cdr i listen. Altsaa car-peker i listen er satt slik at den peker paa resten av listen.
;saa a erstattes med 42 ved alle car posisjoner i listen. 

'--3C--
;(define(cycle? liste)
 ; (define(iterere t-liste h-liste)
  ;  (cond (null? ))
   ; ))

;Kall fra oppgavetekst
;(cycle? '(hey ho))
;(cycle? '(la la la))
;(cycle? bah)
;(cycle? bar)


'--3D--
s

'--3E--


'--3F--



