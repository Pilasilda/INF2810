;; Oblig1a

;;1(a)
;; (*(+ 4 2)5)
;; >(* 6 5)
;; > 30
;;Her er svaret riktig fordi den følger nr 2, den anvender operatoren på riktig plass.
;;Syntax er riktig.

;;(b)
;;(*(+ 4 2)(5))
;; Error, det mangler en argument ved 5.
;; Det er ingen operator for operanden ved 5, derfor bryter den nr 2 i evalueringsregelen.

;;(c)
;;(* (4 + 2) 5)
;;Error, det er syntax-feil ved 4 + 2. Bryter med nr 2 i evalueringsregelen.

;;(d)
;;(define bar (/ 42 2))
;; bar
;; > 21
;; Riktig, pga riktig syntax og bruk av define.

;;(e)
;;(- bar 11)
;; uten define får vi undefined error fordi den ikke vet hvor bar kommer fra.
;; da bryter den med 1b) at man ikke kan evaluere til verdien de refererer til.
;; med define fra d) så får vi > 10.

;;(f)
;;(/ (* bar 3 4 1) bar)
;;uten define får vi undefined error fordi den ikke vet hvor bar kommer fra.
;;1b) kan ikke evalueres til verdien de referer til.
;; med define fra d) så får vi > 12.

;; 2(a)
;; or er en 'special form' som tester om hvis minst en av tilfellene er sant.
;; Den evaluerer tilfellene en etter en etter rekkefølgen de er skrevet på.
;; Hvis første tilfelle er sant så vil den returnere resultatet med en gang,
;; og ignorere resten av tilfellene. Her ser man at den bryter med evalueringsregelen
;; til Scheme som sier at man evaluerer alle argumentene.


;;(or (= 1 2)
;;    "piff!"
;;    "paff!"
;;    (zero? (1 - 1)))


;; 1 er ikke lik 2 så vi hopper over det tilfelle
;; Siden "piff!" ikke er false, så returnerer or denne ut og ignorerer resten av tilfellene.
;; Syntaksfeilen her er at operatoren ved siste tilfelle er skrevet feil, den skal ligge lengst til venstre.
;; Men siden dette tilfelle blir ignorert blir det ikke noe error. Samme gjelder ved and.

;; and er en 'special form' som tester om hvis alle tilfellene er sanne.
;; Den vil evaluere alle tilfeller her dersom alle er sanne, men hvis en av
;; tilfellene er usanne så vil den returnere resultatet med en gang og ignorere
;; resten av tilfellene. Her bryter den med evalueringsregelen til Scheme som sier
;; at man må evaluere alle argumentene.

;;(and (= 1 2)
;;     "piff!"
;;     "paff!"
;;     (zero? (1 - 1)))


;; Her returnerer det false fordi første tilfelle er false og den ignorerer resten

;; if er en 'special form' som tester om tilfelle er riktig også returnerer den
;; enten true eller false etter hva man har referert det til.

;;(if (positive? 42)
;;"poff!"
;;(i-am-undefined))

;; if sjekker om 42 er positiv. Det er true.
;; Derfor returnerer den "poff!", fordi det refereres til true, mens
;; (i-am-undefined) refereres til false.
;; Siden den ignorerer det siste tilfelle får den ikke sjekket om det er feil eller ikke
;; hvis tilfelle var falskt så ville den hoppet til i-am-undefined og fått med at den ikke er refererbar.


;; 2(b)
;; if
(define sign1
  (lambda (x)
    (if (= x 0)
        0
    (if (positive? x)
        1
        -1
        ))))

;; cond
(define sign2
  (lambda (x)
    (cond ((> x 0) 1)
          ((< x 0) -1)
          (else 0))
    ))


;; 2(c)
(define sign3
  (lambda (x)
    (or (positive? x)
        (and (zero? x)
        "0"))))

;; true skal stå for positiv tall
;; false skal stå for negativ tall
;; "0" skal stå for null.
;; Først sjekker den om tallet er positiv, hvis den er positiv så får vi true
;; Hvis ikke utelukker vi det og går videre til neste tilfelle
;; Her sjekker den om det er 0, hvis den ikke er 0 får vi false
;; Hvis den er null derimot skal det printes ut "0".

;; 3(a)
;; add1
(define add1
  (lambda (x)
    (+ x 1)))

;; sub1
(define sub1
  (lambda (x)
    (- x 1)))

;; 3(b)
(define plus
  (lambda (x y)
    (if (zero? x)
        y
        (plus (sub1 x)(add1 y))  
    )))

;; 3(c)
;; Prosedyren i oppgave 3 b er rekursiv fordi den anvender seg selv i sin egen definisjon.
;; Her er prosedyren plus også brukt innenfor prosedyren.
(define plus2
  (lambda (x y)
    (if (> 1 x)
        y
        (plus (sub1 x)(add1 y))
        )))

;; Denne prosedyren er iterativ fordi den har en repeterende prosess.
;; Den er derimot også rekursiv siden den kaller på seg selv innenfor en prosedyre.


;; 3(d)
(define (power-close-to b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (+ 1 e))))
(power-iter 1)
  )

;; Det blir blokkstruktur ved å skrive power-iter prosedyren innenfor power-close-to
;; Man kan forkorte prosessen ved å fjerne b og n fra power-iter siden det allerede
;; er definert ved power-close-to og siden prosedyren ligger innenfor den kan
;; den få tilgang til disse variablene.

;; 3(e)
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
(fib-iter 1 0 n)
  )

(fib 10)

;; Nei, det er fordi det er ingenting å forkorte
;; Selv når man lager blokkstruktur og fib-iter får tilgang til n så trenger
;; den fortsatt de samme tilfellene for å beregne Fibonacci-tall.


