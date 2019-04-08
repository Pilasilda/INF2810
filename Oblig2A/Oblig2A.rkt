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


;Evaluerer til (towel(42 towel)), fra oppgavetekst
(let ((bar foo)
  (baz 'towel))
 (let ((bar (list bar baz))
       (foo baz))
  (list foo bar)))


;Evaluerer til 

;Oppgave 1c


;Oppgave 2a
(define (member? eq? x liste)
  (if(null? liste)
     #f
     (if(eq? x (car liste))
        #t
        (member? eq? x (cdr liste)))))

;Kall eksempeler
;(member? eq? 'zoo '(bar foo zap))
;(member? eq? 'foo '(bar foo zap))
;(member? = 1 '(3 2 1 0))
;(member? eq? '(1 bar)'((0 foo) (1 bar) (2 baz)))
;(member? equal? '(1 bar)'((0 foo) (1 bar) (2 baz)))

;Dekode prosedyre - halerekursiv
(define (decode bits tree)
  (define(decode-1 bits current-branch)
    (define(choose-branch bit branch)
      (if (= bit 0)
          (left-branch)
          (right-branch)))
    (if(null? bits)
       '()
       (let ((next-branch
              (choose-branch (car bits) current-branch)))
         (if (leaf? next-branch)
             (cons (symbol-leaf next-branch)
                   (decode-1 (cdr bits) tree))
             (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(decode sample-code sample-tree)







