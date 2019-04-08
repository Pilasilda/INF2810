where'--1f--
(define foo(list 0 42 #t list))
(car(cdr foo))


'--1g--
(define foo2 (list (list 0 42) (list #t list)))
(car(cdr (car foo2)))


'--1h--
(define foo3 (list(list 0)(list 42 #t)(list list)))
(car(car(cdr foo3)))


'--1i--
;deloppgave (g) ved bruk av list
(list(list 0 42) (list #t list)) 

;deloppgave (g) ved bruk av list 
(cons (cons 0 (cons 42 '())) (cons (cons #t (cons list '())) '())) 


'--2a--
(define(length2 items) ;;definerer liste
  (define (length3 items x) ;; liste og variabel/teller 
    (if(null? items) ;;hvis element i liste er null 
       x ;;return element 
       (length3 (cdr items) (+ 1 x)))) ;;hvis ikke de øvrige forholdene; reduserer lista med cdr, lengden av listen er 1+ uten første element siden x er tellern så 1+x 
  (length3 items 0)) ;; x=0, start 

;;Kall
(length2 '(4 5 6 8))


'--2b--

(define(reduce-reverse proc init liste)
  (define(reduce-reverse1 a b liste)
    (if(null? liste)
       b
       (reduce-reverse1 a(a (car liste) b) (cdr liste))))
  (reduce-reverse1 proc init liste))

;;Kall
;(reduce-reverse + 0 '(1 2 3))
;(reduce-reverse cons '() '(1 2 3))

;;Dette er en halerekursiv versjon fordi listen blir lagd for hvert element vi går gjennom.


;;Oppgave 2c
(define (all? pred liste)
  (cond((null? liste) ;;hvis listen er null returner true 
       #t)
       (else(and (pred (car liste)) ;;hvis ikke liste sin første element  
            (all? pred(cdr liste)))))) ;;liste sin resterende elementer 


;(all? odd? '(1 3 5 7 9))


(define (all?2 pred liste)
  (cond((null? liste) #t)
  ((< 10 (car liste)) #f)
       (else(and (pred (car liste)) ;;hvis ikke liste sin første element  
            (all?2 pred(cdr liste)))))) ;;liste sin resterende elementer 
;(all?2 odd? '(1 3 5)) 


;;Oppgave 2d

(define (nth a liste) ;;prosedyre nth som tar inn to argumenter en indeks tall og en liste
  (if(= 0 a) ;;hvis a er lik 0 
     (car liste) ;; returner liste sin første element 
     (nth (- a 1) (cdr liste)))) ;;ellers return liste sin element -1 og liste sin rest elementer 

;;Kall fra oppgavetekst 
; (nth 2 '(47 11 12 13))


;;Oppgave 2e
(define (where a liste) ;;prosedyre where som tar inn tall som argument og en liste. a representerer tallet vi ønsker å hente ut. 
  (define (teller liste b) ;;prosedyre med teller samme liste og tall b. b representerer indeks plass. 
  (if (null? liste) ;;hvis listen er null return false 
      #f
      (if (= a (car liste)) 
             b
             (teller (cdr liste) (+ 1 b)))))
      (teller liste 0))

;;Kall som tester prosedyren over. 
;(where 3 '(1 2 3 3 4 5 3)) ;;returnerer 2, fordi vi ønsker 3 å få ut hvilken plass 3 ligger i lista. Første 3 ligger på indeks 2 og returnerer derfor dette. 
;(where 0 '(1 2 3 3 4 5 3));; returnerer false fordi, 0 ikke er i listen. 



;;Oppgave 2f
(define (map2 proc liste liste2) ;;prosedyre map2 med to lister som parameter
  (cond
    ((null? liste) '()) ;;hvis element i liste er 0 return den tomme listen
    ((null? liste2) '()) ;;hvis element i liste2 er 0 return den tommelisten 
    (else(cons (proc (car liste) (car liste2)) ;;adderer første element i liste med første element i liste2
               (map2 proc (cdr liste) (cdr liste2)))))) ;;adderer cdr som er de resterende elementer i liste med cdr som er resterende elementer i liste2

;Kall
;(map2 + '(1 2 3 4) '(3 4 5)) 



;;Oppgave 2g
(map2(lambda(a b)(/(+ a b)2))'(1 2 3 4)'(3 4 5)) ;;map2 kombinert med anonymprosedyre som beregner gjennomsnitt av to tall
;;kaller først på map2, oppretter så et lambdauttrykk hvor jeg sender med to argumenter a og b. Så definerer jeg hvordan prosedyre skal gjøre utregning, altså først summere a og b så dele dette på 2. 



;;Oppgave 2h
(define (both? pred) ;;prosedyre both? som tar inn et predikat 
  (lambda (a b) ;;predikatet som generer to argumenter 
   (and (pred a) (pred b)))) ;;siden and som specialform tester om alle forholdene i uttrykket er sanne så tester den først om element (a) er sann også sjekker den som element (b) er sann. 

;;Kall på both? prosedyren
;(map2 (both? even?) '(1 2 3) '(3 4 5))
;((both? even?) 2 4)
;((both? even?) 2 5)

;;siden returnverdien til both? er en prosedyre som returnerer #t dersom tilfellet er riktig for begge argumentene, så er jeg usikker på om det er da riktig å bruke and, siden both? vil sjekke for begge forholdene. 



;;Oppgave 2i
(define(self proc) ;;prosedyre self som tar inn proc som argument, 
  (lambda(a) (proc a a))) ;; returnerer en ny prosedyreobjekt lambda som sender med argument a.

;;proc tilsvarer her stortsett en operator som vi sender med ved et kall. 


;;Kall
;;((self +) 5) ;;adderer 5+5 som gir 10 
;;((self *) 3) ;;multipliserer 3 med 3 
;;(self +) 
;((self list) "hello") ;;gir en liste med to hello i paranteser. 


















