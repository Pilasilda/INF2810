;I denne obligen har Truc-my(tmngo) og Pilasilda(pilasila) samarbeidet. 

;;Oblig 2b
'----1a----
;Oppg 1(a)
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1)) count)))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

;Kall
(display "(c1) --> ") (c1)
(display "(c1) --> ") (c1)
(display "(c1) --> ") (c1)

(display "count --> ") count
(display "(c2) --> ") (c2)

;Oppg 1(b)

'----2a----
;Oppg 2(a)
(define (make-stack x)
  (let ((stack x))
    (lambda (msg . args)
      (cond
        ((eq? msg 'pop!) (if (null? x)
                             (set! stack x)
                             (set! stack (cdr stack))))
        ((eq? msg 'push!) (set! stack (append (reverse args) stack)))
        ((eq? msg 'stack) stack)
      (else "Not valid message!")))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

;Kall
(s1 'pop!)
(display "(s1 'stack) --> ")(s1 'stack)
(s2 'pop!)
(display "(s2 'push! 1 2 3 4) --> ")(s2 'push! 1 2 3 4)
(s2 'stack)
(s1 'push! 'bah)
(display "(s1 'push! 'zap 'zip 'baz) --> ")(s1 'push! 'zap 'zip 'baz)
(s1 'stack)

'----2b----
;Oppg 2(b)
(define pop!
  (lambda (x)
    (x 'pop!)))

(define stack
  (lambda (x)
    (x 'stack)))

(define push!
  (lambda (x . args)
    (apply x 'push! args)))

(pop! s1)
(display "(stack s1) --> ")(stack s1)
(push! s1 'foo 'faa)
(display "(stack s1) --> ")(stack s1)

;Oppg 3(a)
; 1. liste-ref er en innebygd prosedyre som peker paa indeksen sin sitt symbol i listen.
; 2. (cdddr bar) peker til 'd, (cdr bar) peker tilbake til 'b, her lages det derfor en syklisklist fordi d vil alltid peke tilbake til b.
; 3. d sin indeks vil alltid vaere b.

;Oppg 3(b)
;Aarsaken til at bah evaluerer til ((42 towel) 42) er fordi verdien car peker paa i bah settes til aa peke
;det samme objektet som cdr i listen. Altsaa car-peker i listen er satt slik at den peker paa resten av listen.
;Dermed erstattes element i car-posisjon med 42 ved alle car-posisjoner. 

'----3c----
;Oppg 3(c)
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))

(define (cycling head tail)
  (cond
    ((null? tail) #f)
    ((null? (cdr tail)) #f)
    ((eq? head tail) #t)
  (else (cycling (cdr head)(cddr tail)))))

(define (cycle? lst)
  (if (null? lst)
      #f
      (cycling lst (cdr lst))))

(display "(cycle? '(hey ho) --> ")(cycle? '(hey ho))
(display "cycle? bar) --> ")(cycle? bar)

;Oppg 3(d)
;Lister har ogsaa en '() liste paa enden, mens en sykliskliste mangler dette vil det alltid returnere en verdi fordi det peker tilbake paa et tidligere element. 

'----3e----
;Oppg 3(e)
;Hjelpeprosedyrer
(define lst
  (lambda (x)
    (x 'lst)))

(define top
  (lambda (x)
    (x 'top)))

(define left-rotate!
  (lambda (x)
    (x 'left-rotate!)))

(define right-rotate!
  (lambda (x)
    (x 'right-rotate!)))

(define insert!
  (lambda (x . e)
    (apply x 'insert! e)))

(define delete!
  (lambda (x)
    (x 'delete!)))

(define (right-rotate lst)
  (let ((l (reverse lst)))
   (if (null? lst)
      '()
      (cons (car l)(reverse (cdr l))))))

;Hovedprosedyren
(define (make-ring lst)
  (lambda (msg . element)
    (cond
      ((eq? msg 'top) (car lst))
      ((eq? msg 'right-rotate!) (set! lst (right-rotate lst))(car lst))
      ((eq? msg 'left-rotate!) (set! lst (append (cdr lst) (cons (car lst)'())))(car lst))
      ((eq? msg 'insert!) (set! lst (append element lst))(car lst))
      ((eq? msg 'delete!) (set! lst (cdr lst))(car lst))
      ((eq? msg 'lst) lst)
      (else "Not valid message!")
      )))

(define r1 (make-ring '(1 2 3 4)))
(define r2 (make-ring '(a b c d)))

;Kall
(display "(top r1) --> ")(top r1)
(display "(top r2) --> ")(top r2)
(display "(right-rotate! r1) --> ")(right-rotate! r1)
(display "(left-rotate! r1) --> ")(left-rotate! r1)
(display "(left-rotate! r1) --> ")(left-rotate! r1)
(display "(delete! r1) --> ")(delete! r1)
(display "(left-rotate! r1) --> ")(left-rotate! r1)
(display "(left-rotate! r1) --> ")(left-rotate! r1)
(display "(left-rotate! r1) --> ")(left-rotate! r1)
(display "(insert! r2) --> ")(insert! r2 'x)
(display "(right-rotate! r2) --> ")(right-rotate! r2)
(display "(left-rotate! r2) --> ")(left-rotate! r2)
(display "(left-rotate! r2) --> ")(left-rotate! r2)
(display "(top r1) --> ")(top r1)


'----3f----
;liste '(1 2 3 4)
;Hvis vi sier hodet er (car liste), mens kroppen er resten av listen unntatt for hodet (cdr liste)
;Da er hodet 1 her og kroppen er (2 3 4)

;;left-rotate!
;Jeg tar i bruk append til å slå sammen hale og kroppen slik at vi får flyttet elementene en posisjon til venstre.

;;right-rotate!
;Her har jeg lagd en hjelpeprosedyre som bruker let for å reversere koden slik at det er enklere å få tak i ønskede elementer
;det betyr (4) og (1 2 3)
;Hvis man reverserer listen får man (4 3 2 1) og kan dermed hente elementene gjennom (car lst) og (cdr lst)
;Etter at man har fått elementene så må man sette dem på riktig rekkefølge igjen, så man må igjen reversere kroppen før man slår dem sammen

;;insert!
;her er det bare å slå sammen elementet med listen

;;delete!
;her er det bare å sette den nye listen lik som (cdr lst) kroppen til listen og da utelater man hodet som tilsvarer "top" av listen.