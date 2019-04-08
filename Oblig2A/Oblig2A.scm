(load "huffman.scm")

;Oppgave 1a
;Selektoren p-cons 
(define (p-cons x y)
  (lambda (proc) (proc x y)))
;Kall
;(p-cons "foo" "bar")

;Selektoren p-car 
(define (p-car proc)
  (proc (lambda (x y) x)))
;Kall
;(p-car (p-cons "foo" "bar"))

;Selektoren p-cdr
(define (p-cdr proc)
  (proc (lambda (x y) y)))
;Kall 
;(p-cdr(p-cons "foo" "bar"))

;Kall
;(p-car (p-cdr (p-cons "zoo" (p-cons "foo" "bar"))))



;Oppgave 1b
(define foo 42)
;Evaluerer til different: fra oppgavetekst 
(let  ((foo 5)
       (x foo))
 (if (= x foo)
     'same
     'different))


;Evaluerer til different 
((lambda (foo x)
   (if(= x foo)'same 'different))foo 5)


;Let-uttrykk: evaluerer til (towel(42 towel)), fra oppgavetekst
(let ((bar foo)
  (baz 'towel))
 (let ((bar (list bar baz))
       (foo baz))
  (list foo bar)))

;Lambda-uttrykk: evaluerer også til (towel(42 towel))
((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz)baz))foo 'towel) 



;Oppgave 1c
(define(infix-eval exp) ;definerer prosedyre infix-eval
  (let((operand1 (car exp)) ;definerer variabel 1 som skal være det første elementet i lista. 
       (operator (car(cdr exp))) ;definerer operatoren som i dette tilfellet er + - * eller / 
       (operand2 (car (cdr (cdr exp))))) ;definerer operand2 som skal være det elementet som skal adderes, multi, sub eller divderes med 
    (operator operand1 operand2))) ;selve regnestykket, hvor man + - * eller / operand1 med operand2 

;Kall fra oppgavetekst 
(define foo (list 21 + 21)) ;kallet gir 42 som svar
(define baz (list 21 list 21));kallet gir (21 21)
(define bar (list 84 / 2)) ;kallet gir 42 som svar 

;(infix-eval foo)
;(infix-eval baz)
;(infix-eval bar)



;Oppgave 1d
(define bah '(84 / 2))
;(infix-eval bah)
;Resultatet etter å ha kjørt dette er "not a procedure", grunnen til dette er fordi vi sender med ' - quote. En (') er et symbol og ikke en prosedyre, det forventes at det skal gis en liste med tall
;og ikke en liste som inneholder tall og symbolet ('). 



;Oppgave 2a
;Prøvde to forskjellige måter, først en hvor jeg sender med eq? som argument 
(define (member? eq? x liste)
  (if(null? liste)
     #f
     (if(eq? x (car liste))
        #t
        (member? eq? x (cdr liste)))))


;Så en versjon hvor jeg kalte predikat for eqpred, kallet avgjør om predikatet er
; av typen eq? eller equal? eller av annen type predikat 
(define(member? eqpred x liste);definerer member sender inn argument eqpred, liste og et element x 
  (cond((null? liste)#f) ; hvis listen er null returnere false 
       ((eqpred x (car liste)) #t) ;hvis elementet finnes i lista returnere, true
       (else (member? eqpred x (cdr liste)))));hvis ikke de førstnevnte 

;Kall eksempeler
;(member? eq? 'zoo '(bar foo zap)) ; gir false 
;(member? eq? 'foo '(bar foo zap)) ; gir true
;(member? = 'foo '(bar foo zap)) ; gir error 
;(member? = 1 '(3 2 1 0)) ; gir true 
;(member? eq? '(1 bar)'((0 foo) (1 bar) (2 baz))) ; gir false 
;(member? equal? '(1 bar)'((0 foo) (1 bar) (2 baz))) ; gir true



;Oppgave 2b
(define (decode-halerek bits tree)
  (define (decode-1 bits current-branch liste)
    (if(null? bits)
       liste
       (let ((next-branch
              (choose-branch (car bits) current-branch)))
         (if(leaf? next-branch)
            (decode-1 (cdr bits) tree (append liste(cons(symbol-leaf next-branch)'())))
            (decode-1 (cdr bits) next-branch liste)))))
    (decode-1 bits tree '()))


(define (decode2 bits tree)
  (define  (decode-iter bit current-branch code)
    (if (null? bit)
        (reverse code)
        (let ((next-branch
               (choose-branch (car bit) current-branch)))
          (if (leaf? next-branch)
              (decode-iter (cdr bit)
                           tree
                           (cons (symbol-leaf next-branch) code))
              (decode-iter (cdr bit) next-branch code)))))
  (decode-iter bits tree '()))



;Oppgave 2c
;Kall: resultat av kode (ninjas fight ninjas by night)
;(decode sample-code sample-tree)
;(decode-halerek sample-code sample-tree)



;Oppgave 2d
(define (encode msg tree)
  (if (null? msg)
      '()
      (append (encode-symbol (car msg) tree)
	      (encode (cdr msg) tree))))

(define (encode-symbol symbol tree)
  (define (encode-1 t encoded)
    (if (leaf? t)
	(reverse encoded)
	(let ((left (left-branch t))
	      (right (right-branch t)))
	  (let ((symbols-left (symbols left))
		(symbols-right (symbols right)))
	    (cond ((member? eq? symbol symbols-left)
		   (encode-1 left (cons 0 encoded)))
		  ((member? eq? symbol symbols-right)
		   (encode-1 right (cons 1 encoded)))
		  (else
		   (symbol)))))))
  (encode-1 tree '()))
      

;Oppgave 2e
(define (grow-huffman-tree frekvens)
  (define (grow-huffman-tree2 frek)
    (if (=(length frek) 1)
        (car frek)
        (let ((first (car frek))
              (second (cadr frek))
              (rest (cddr frek)))
          (grow-huffman-tree2 (adjoin-set (make-code-tree first second)
                                          rest)))))
  (grow-huffman-tree2 (make-leaf-set frekvens)))


(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
;(decode (encode '(a b c) codebook) codebook)
;(encode '(a b c) codebook)

;Kall fra oppgavetekst
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(define(encode '(a b c) codebook)codebook)

;Oppgave 2f
;(define alfabetet '((samurais 57)(ninjas 20)(fight 45)
 ;                                (night 12)(hide 3)(in 2)(ambush 2)(defeat 1)
  ;                               (the 5)(sword 4)(by 12)(assassin 1)(river 2)
   ;                              (forest 1)(wait 1)(poison 1)))

;(encode '(ninjas fight
 ;                ninjas fight ninjas
  ;               ninjas fight samurais
   ;              samurais fight
    ;             samurais fight ninjas
     ;            ninjas fight by night))

;Oppgave 2g
(define(huffman-leaves tree) ;lager en prosedyre huffman sender med treet som 
  (define(huffman2 h) ;lager en hjelpe prosedyre med variabel h 
  (list(list (symbol-leaf h)(weight-leaf h)))) ;lager en liste med kall på symbol-leaf prosedyre som gir symboler i treet og en liste
  ;med weight av treet, slår så begge listene sammen til en 
    (if(leaf? tree) ;hvis det er en gren i treet
     (huffman2 tree);så henter den listen med symboler og weight
      (cons(huffman-leaves(left-branch tree)) 
            (huffman-leaves (right-branch tree)))))

;Kall fra oppgavetekst, returnerer ((ninjas 8) (fight 5) (night 1) (by 1))
(huffman-leaves sample-tree)
















