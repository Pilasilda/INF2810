(load "prekode3a.scm")

;Obligatorisk oppgave for Trucmy Ngo Bao og Pilasilda A.George 

'--Oppgave1A&1B--
(define tabel1 (make-table)) 

(define (mem melding proc) ;definerer mem som sender med melding og proc som argument 
  (define (memoize) ;definerer  memoize 
    (let ((table (make-table))) ;oppretter tabellen verdiene skal legges i
      (lambda args ;opretter lambda-uttrykk med argumenter uavhengig av hvor mange som sendes inn, derfor args
        (let ((prev (lookup args table))) ;lookup returnerer true eller false om verdiene finnes eller ikke, args gir svaret av fib som er definert i lambda, table er tabellen som verdiene skal lagres i.
          ;I prev lagres alle verdiene som er i parantesen 
          (or prev 
              (let ((resultat (apply proc args))) ;Slår sammen fib verdiene (ex. computing  3 2 1 0) og svar av fib legger dette i resultat
                (insert! args resultat table) resultat)))))) ;legger til verdi i tabellen 
  (define (unmemoize) ;definerer prosedyren unmemoize 
    (or (lookup proc tabel1) proc)) ;lookup returner som sagt true eller false så hvis verdien finnes i tabellen så returneres true hvis ikke false
     ;proc er fib, proc verdien returneres som altså er fib verdiene 
  (cond ((equal? melding 'memoize) ;en sjekk som tester om meldingen er lik 'memoize
         (let ((previous proc) ;lar "previous" være prosedyren som tas inn i kallet på memoize og "new" være memoize prosedyrer
               (new (memoize)))
           (insert! new previous tabel1) new)) ;inserter kallet på memoize (new) som nøkkel og prosedyren som tas inn som tas inn (previous) som verdi
           ;før let'en avsluttes, returneres new som er kallet på memoize
        ((equal? melding 'unmemoize) (unmemoize))))  ;hvis meldingen er lik 'unmemoize kalles unmem prosedyren 


;Kall fra oppgavetekst 
(set! fib(mem 'memoize fib))
(fib 3)
(fib 3)
(fib 3)
(fib 3)
(fib 2)
(fib 4)

(set! fib(mem 'unmemoize fib))
(fib 3)

;Kall fra oppgavetekst 
(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)
(test-proc 42 43 44)


'--Oppgave1C--
;Kall fra oppgavetekst
(define mem-fib(mem 'memoize fib))
(mem-fib 3)
(mem-fib 3)
(mem-fib 2)
(mem-fib 1)

#|Det som skjer ved kall på (mem-fib) er at kun det første resultatet av kallet blir skrevet ut,
så hvis vi kaller på (mem-fib 3), så er det kun (fib 3) som lagres i cachen, ikke (fib 2) eller (fib 1) som i det forste eksempelet.
Årsaken til dette er at vi ikke endrer bindingen(-) i fib prosedyren, i det første eksemplet setter vi fib til å være (mem 'unmemoize fib) f.eks. men det gjør vi ikke i
dette eksempelet. Dermed vil prosedyren kun memoisere verdien som blir sendt inn men de resterende kallene på fib prosedyren. 
|# 


'--Oppgave1D

;;Hjelpemetode
(define (hjelp argdefs args proc)
  (apply proc
         (map (lambda (argdef)
                (let loop ((args args))
                  (cond ((null? args)
                         (cadr argdef))
                        ((eq? (car argdef) (car args))
                         (cadr args))
                        (else
                         (loop (cdr args))))))
              argdefs)))

;;Greet-metoden
(define (greet . args)
  (hjelp '((time "day")(title "friend"))
                 args
                 (lambda (time title)
                   (display "good ")
                   (cond (time
                          (display time)
                          (display " ")
                          (display title)))
                   (newline))))

(greet) ;kaller paa greet 
(greet 'time "evening") ;kaller paa greet og sender meg evening som argument, 'time er den navngitte argumentet
(greet 'title "sir" 'time "morning");forste som vises er good, saa kalles buffer i greet da hopper vi til den prosedyren, 
(greet 'time "afternoon" 'title "dear")



'--Oppgave2A--
(define (list-to-stream list)
  (if (stream-null? list)
      the-empty-stream
      (cons-stream (stream-car list) (stream-cdr list))))

;(list-to-stream '(1 2 3 4 5))

;;

(define (stream-to-list stream . n)
  (define (stl stream i)
    (if (or (= i 0) (stream-null? stream))
        the-empty-stream
        (cons (stream-car stream)
                      (stl (stream-cdr stream) (- i 1)))))
  (stl stream (if (null? n) 15 (car n))))

;(stream-to-list (stream-interval 10 20))

(define (show-stream stream . n)
  (define (ss-rec stream i)
    (cond ((= i 0) (display "...\n"))
          ((stream-null? stream)  (newline))
          (else (display (stream-car stream))
                (display " ")
                (ss-rec (stream-cdr stream) (- i 1)))))
  (ss-rec stream (if (null? n) 15 (car n))))
;;

;(show-stream nats 15)
;(stream-to-list nats 10)

'--Oppgave2B--
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

'--Oppgave2C--
;;En av problemene som kan oppstaa er naar man bruker strommer i memq, fordi strommene er uendelige og vil derfor fore til at man potensielt ikke faar avsluttet prosedyren.
;;Dermed faar man ikke ut noen svar.


'--Oppgave2D--
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter (lambda(x) (not (eq? x (stream-car stream))))
                                   (stream-cdr stream))))))

'--Oppgave2E--

(define x    
       (stream-map show
                   (stream-interval 0 10))) ;bare forste element skrives ut her er 0 det forste elementet i intervallet og skrives dermed ut
     (stream-ref x 5) ;tallene mellom 1-5 skrives ut fordi stream ref skriver ut. Men verdien 0 har allerede blitt skrevet ut tidligere som vil si memoizert derfor skrives kun 1-5 ut 
     (stream-ref x 7) ;her skrive tallet 6 og 7 ut, fordi de andre verdiene har allerede blitt skrevet ut, 1-5 er skrevet ut tidligere saa kun 6 og 7 skrives ut. 

'--Oppgave2F--
(define (mul-streams s1 s2)
  (cons-stream
   (* (stream-car s1)(stream-car s2))
   (mul-streams (stream-cdr s1)(stream-cdr s2))))


'--Oppgave2G--
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))

(stream-ref factorials 5)




