(load "evaluator.scm")
;Truc-my Ngo Bao & Pilasilda A.George

;Den globale omgivelsen, omgivelse er en sekvens av ramme(tabell), mens den globale omgivelsen er en enkelt ramme 
(set! the-global-environment (setup-environment))

(read-eval-print-loop)

;For oppgave 3
;Kode fra prekode-evaluator for oppgave A, C, E
(define (eval-special-form exp env)
  (cond ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (mc-eval (cond->if exp) env))
        ;legger til for and og or for oppgave 3A
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ;legger til let for oppgave 3C 
        ((let? exp) (mc-eval (let->lambda exp) env))
        ;legger til while for oppgave 3E 
        ((while? exp) (eval-while (cdr exp) env))))

;Kode fra prekode-evaluator
(define (special-form? exp)
  (cond ((quoted? exp) #t)
        ((assignment? exp) #t)
        ((definition? exp) #t)
        ((if? exp) #t)
        ((lambda? exp) #t)
        ((begin? exp) #t)
        ((cond? exp) #t)
        ;and og or for oppgave 3A:
      ;  ((or? exp )#t) ;legger til ny sjekk for or som tester or som special form, oppgave 3a
       ; ((and? exp) #t);legger til ny sjekk for and som tester and som special form, oppgave 3a
        ;legger til let for oppgave 3C 
        ;((let? exp) #t)
        ;while for oppgave 3E
        ;((while? exp)#t) 
        (else #f)))



'--Oppgave1A--
;(define (foo cond else)
 ; (cond ((= cond 2) 0)
  ;      (else (else cond)))) ; -->  1) printer ut prosedyren som skal evalueres og ok. 

(define cond 3) ; -->  2) ok

(define (else x) (/ x 2)) ; --> 3) ok

(define (square x) (* x x)) ; --> 4) ok

;(foo 2 square) ; --> 5) 0, Årsaken til at uttrykk 5 evalueres til 0 er fordi at vi tidligere har definert en prosedyre foo som sjekker om cond=2 og da ved
;uttrykk 5 så sjekker vi er cond=2 ja det er den og derfor returneres 0.

;(foo 4 square) ; --> 6) 16, (= cond 2) går ikke igjennom derfor går prosedyren til else hvor (square 4) gir 4 * 4 som er 16

;(cond ((= cond 2) 0)
 ;     (else (else 4))) ; --> 7) 2, (= cond 3) er definert over så (= cond 2) vil ikke slå til da går prosedyren til else og utfører 4/2 -> 2. 

#|Svaret som returneres etter hvert uttrykk er testet og gitt ved kommentar bak hvert uttrykk. I disse oppgavene er det viktig å kunne se forskjell mellom
cond som er innebyd og cond som kun er definert som en vanlig argument/variabel. Selv om den innbygde conden og conden vi definerer er like så er de lagret i forskjellige plasser
dette gjør det også mulig for scheme å se forksjellen mellom de to condene. Itillegg tror vi at måten uttrykkene er satt opp på er med på avgjøre hvordan uttrykket skal evalueres,
hvis da cond eller else er satt opp på måten cond som specialform skal settes opp så vil scheme tolke det utifra dette. På samme måte hvis den ikke
er satt opp på måten cond eller else er satt opp som specialform vet den da at det ikke er den innebygde formen som blir tatt i bruk, og da vil den også finne
argumentet som hører til eller variabelen for den slags skyld.Et annet viktig punkt er at når symboler defineres vil de lagres i tagged-list med quote ' foran, dermed cond og else vil kunne bli evaluert som special form. 

Uttrykkene fra 1-4 er kun prosedyrene som defineres på forhånd, hvis vi taster disse inn i  REPL for mc-eval så får vi OK som tilbakemelding etter
hvert av uttrykkene er evaluert dette tolker vi som at prosedyren er godkjent og kan testes opp mot kallet som gjøres ved uttrykk 5-7. 
 |#

'--Oppgave2A--
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'display 
              (lambda (x) (display x) 'ok))
        (list 'newline 
              (lambda () (newline) 'ok))
        (list '1+  
              (lambda (x) (+ x 1)) ;lambda uttrykk som adderer x med 1 
        (list '1-
              (lambda (x) (- x 1)))))) ;lambda uttrykk som subtraherer x med 1 

;Ved MC-eval input (1+ 2) --> MC-eval value: 3
;Ved MC-eval input (1- 2) --> MC-eval value: 1 
(set! the-global-environment(setup-environment))



'--Oppgave2B--
(define (install-primitive! exp name)
  (define-variable! exp (list 'primitive name) the-global-environment))

 
;Kall fra oppgavetekst 
(install-primitive! 'square (lambda (x) (* x x)))
(mc-eval '(square 4) the-global-environment)

;Tester ved å skrive inn ('sqaure 4) i replet --> 16 


'--Oppgave3A--
;legger til i tagged-list fordi and er special form return: and
(define (and? exp) (tagged-list? exp 'and))
;legger til i tagged-list forid or -> special form return: or 
(define (or? exp) (tagged-list? exp 'or))

;Prosedyre som evaluerer and 
(define (eval-and exp env)
  (define (iter liste)
   (if (null? liste)
       #t
       (if (false? (mc-eval (car liste) env))
           #f
           (iter (cdr liste)))))
  (iter (cdr exp)))

;Prosedyre som evaluerer or
(define (eval-or exp env)
  (define (iter liste)
    (if (null? liste)
        #f
        (if (mc-eval (car liste) env)
            (mc-eval (car liste) env)
            (iter (cdr liste)))))
  (iter (cdr exp)))


;Kall for and 
;(mc-eval '(and (= 3 3)(= 4 5)) the-global-environment)
;(mc-eval '(and (= 5 5)(= 7 7)) the-global-environment)

;Kall for or 
;(mc-eval '(or (= 2 4) (= 2 2)) the-global-environment)
;(mc-eval '(or (= 2 3)(= 4 5)) the-global-environment)
;(set! the-global-environment(setup-environment))



'--Oppgave3B--
;Prekode fra evaluator
(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp)env))
      (mc-eval (if-consequent exp)env)
      (mc-eval (if-alternative exp) env)))

;tagged-list for else som returerer else hvis else 
(define (else? exp) (tagged-list? exp 'else))
;if-else -> (car (cdr))
(define (if-else exp) (cadr exp))
;then -> (car (cdr (cdr (cdr))))
(define (then-consequent exp) (cadddr exp))
;if ->(cdr (cdr (cdr (cdr))))
(define (if-alternative exp) (cddddr exp))

;prosedyre eval-if 
(define (eval-if exp env)
  (cond
    ((else? exp) (mc-eval (if-else exp) env))
    ((true? (mc-eval (if-predicate exp) env))
         (mc-eval (then-consequent exp) env))
        (else (eval-if (if-alternative exp) env))))

#| tester if på denne formen
    (if <test1>
    then <utfall1>
    elsif <test2>
    then  <utfall2>
    else <utfall3>)
|# 


;Kall
(mc-eval '(if? (= 3 4) 'then "yo" 'elsif (= 5 2) 'then "yes" 'else "...")the-global-environment)
;Kjører denne i replet (if #f then #f elsif #t then #t else #f) --> #f




'--Oppgave3C--
;let for denne oppgaven lagt til i oppgave 3a, se special-for og eval-special form
(define (let? exp) (tagged-list? exp 'let)) ;returnerer let hvis expression er typen let

(define (let->lambda exp)
  (append
  (list (make-lambda (let-variables (cadr exp)) (list (caddr exp))))
        (let-value (cadr exp))))

(define (let-variables exp)
  (if (null? exp)
      '()
      (cons (caar exp)(let-variables (cdr exp)))))


(define (let-value exp)
  (if (null? exp)
      '()
      (cons (cadar exp) (let-value (cdr exp)))))

;Kall
(mc-eval '(let ((a 1) (b 2) (c 3)) (+ a b c)) the-global-environment)



'--Oppgave3D--
;let i tagged-liste
(define (let? exp) (tagged-list exp 'let))

#| <<<SLETT>>>

(define (let->lambda exp)
  (define (iter expression value variabel)
    (if (null? expression)
        (display "No input")
        (if (not (eq? (car expression) 'in))
            (begin (cons (cadr expression) value)
                   (cons (cadr (cddr expression)) variabel)
                   (iter (cddddr expression) value variabel))
            (cons (make-lambda (variabel) (cadr expression))
                  (value)))))
  (iter exp '() '()))
|#


(define (evaluer-let exp)
  (append (list (make-lambda (variabel (cdr exp)) (list (hoved exp)))) (variabel (cddr exp))))

(define (variabel exp)
  (if (tagged-list? exp 'in)
      '()
      (if (tagged-list? exp '=)
          (variabel (cddr exp))
          (if (tagged-list? exp 'and)
              (if (null? exp)
                  '()
                  (cons (car exp) (variabel (cdr exp))))))))

(define (value exp)
  (if (tagged-list? exp 'in)
      '()
      (if (tagged-list? exp '=)
          (make-variabel (cdr exp))
          (if (tagged-list? exp 'and)
              (value (cddr exp))
              (if (null? exp)
                  '()
                  (cons (car exp) (value (cdr exp))))))))


(define (hoved exp)
  (if (tagged-list exp 'in)
      (cadr exp)
      (hoved (cdr exp))))

;Kall fra oppgavetekst 
(mc-eval '(let x = 2 and y = 3 in (display (cons x y)) (+ x y))the-global-environment)



'--Oppgave3E--
;while lagt til i specialform og evalspecial helt overst 
;while i tagged-list return: while 
(define (while? exp) (tagged-list exp 'while))

;evalwhile prosedyyre 
(define (eval-while pred proc)
  (if pred
      (begin
        proc
        (eval-while pred proc)) #f))

;Kall
(mc-eval '(install-primitive! 'while eval-while) the-global-environment)
















