#lang racket
(require syntax/datum)

;(define com-res '(ADV ADC FDS))

;(append '((quote wd)) '(123 2))

;wo
;(with-datum ([(a ...) (list com-res)])
;    (datum (define xxx 'a ...)))

(eq?
'(
 
( ((IIA) (OIA) (RDW))
  ((NOP) (PUP) (RLF)))

( ((NOP) (NOP) (NOP))
  ((NOP) (RRT) (OIA))))

'(   ;glavno polje
    (  ;z0
     ((IIA) (OIA) (RDW) (STP))        ;y0
     ((NOP) (PUP) (RLF) (STP))) ;y1
    (  ;z1
     ((NOP) (NOP) (NOP) (STP))        ;y0
     ((NOP) (RRT) (OIA) (STP))) ;y1
    ))

;IIA program pita za input
;OIA ispiše
;RDW supusti se na red ispod
;RLF krene lijevo
;PUP popne se na sljedecu "stranicu"
;RRT krene desno
;OIA ispiše

(define comm-lst-ddd-test
  '(   ;glavno polje
    (  ;z0
     ((IIA) (OIA) (RDW) (STP))        ;y0
     ((NOP) (PUP) (RLF) (STP))) ;y1
    (  ;z1
     ((NOP) (NOP) (NOP) (STP))        ;y0
     ((NOP) (RRT) (OIA) (STP))) ;y1
    )) ;x0   ;x1   ;x2   ;x3
     


;memorija
(define prog-mem-lst '(0 0 0 0 0 0)) 
(define prog-mem-A-ptr 2)
(define prog-mem-B-ptr 2) 


(define (dyn-eval-xyz comm-lst-ddd)

  ;vrati elem na x y z poz
  (define (get-comm-lst-elem x y z)
    (list-ref (list-ref (list-ref comm-lst-ddd z) y) x))

  ;get vrijednost memorije
  (define (get-prog-mem-val-at-A)
    (list-ref prog-mem-lst prog-mem-A-ptr))
  (define (get-prog-mem-val-at-B)
    (list-ref prog-mem-lst prog-mem-B-ptr))
  
  ;promjena vrijednosti memorije
  (define (set-prog-mem-A! x)
    (set! prog-mem-lst (list-set prog-mem-lst prog-mem-A-ptr x)))
  (define (set-prog-mem-B! x)
    (set! prog-mem-lst (list-set prog-mem-lst prog-mem-B-ptr x)))
  
  (define (change-prog-mem-A-ptr val proc)
    (set! prog-mem-A-ptr (proc prog-mem-A-ptr proc)))
  (define (change-prog-mem-B-ptr val proc)
    (set! prog-mem-B-ptr (proc prog-mem-B-ptr proc)))

  (define (incr-prog-mem-A-ptr x) (change-prog-mem-A-ptr x +))
  (define (decr-prog-mem-A-ptr x) (change-prog-mem-A-ptr x -))

  (define (incr-prog-mem-B-ptr x) (change-prog-mem-B-ptr x +))
  (define (decr-prog-mem-B-ptr x) (change-prog-mem-B-ptr x -))
  
  (define (add-one-prog-mem-A-ptr) (incr-prog-mem-A-ptr 1))
  (define (sub-one-prog-mem-A-ptr) (decr-prog-mem-A-ptr 1))
  
  (define (add-one-prog-mem-B-ptr) (incr-prog-mem-B-ptr 1))
  (define (sub-one-prog-mem-B-ptr) (decr-prog-mem-B-ptr 1))

  (define (swap-prog-mem-ptr-A-B)
    (let ((tmp get-prog-mem-val-at-A))
      (set-prog-mem-A! get-prog-mem-val-at-B)
      (set-prog-mem-B! tmp)))

  ;Display mem
  (define (display-val-int-at-A)
    (display (get-prog-mem-val-at-A)))

  (define (display-val-int-at-B)
    (display (get-prog-mem-val-at-B)))

  ;Input mem
  (define (input-val-int-at-A)
    (set-prog-mem-A! (string->number (read-line (current-input-port) 'any))))
  (define (input-val-int-at-B)
    (set-prog-mem-B! (string->number (read-line (current-input-port) 'any))))
  
  ;...
  (define (add-one x) (+ x 1))
  (define (sub-one x) (- x 1))

  ;možda nece trebat
  (define ns-es (make-base-namespace))

  ;glavna funkcija za interpretaciju 
  (define (dyn-eval-iter x y z comms dir)
    
    ;pozovi dyn-eval-iter sa novim smjerom
    (define (call-dyn-eval-iter-new-dir x y z comms dir step)
      (cond
        ([eq? dir 1] (dyn-eval-iter x (- y step) z comms dir))
        ([eq? dir 2] (dyn-eval-iter x (+ y step) z comms dir))
        ([eq? dir 3] (dyn-eval-iter (- x step) y z comms dir))
        ([eq? dir 4] (dyn-eval-iter (+ x step) y z comms dir))
        ([eq? dir 5] (dyn-eval-iter x y (+ z step) comms dir))
        ([eq? dir 6] (dyn-eval-iter x y (- z step) comms dir))
        (else 404))) ;nebi nikad trebalo doci do tuda

    ;manje za pisat, odradi proc i nastavi dalje
    (define (travel-and-call x y z comms dir proc)
      (proc) (call-dyn-eval-iter-new-dir x y z comms dir 1))
    
    ;interpretiraj naredbu
    (let ((elem (get-comm-lst-elem x y z))) ;uzmi naredbu iz polja
      (cond
        ;DIRECTION
        ([string=? (car elem) "STP"] #t) ;stop
        ([string=? (car elem) "RUP"] ;row up    #1
         (call-dyn-eval-iter-new-dir x y z comms 1 1))
        ([string=? (car elem) "RDW"] ;row down  #2
         (call-dyn-eval-iter-new-dir x y z comms 2 1))
        ([string=? (car elem) "RLF"] ;row left  #3
         (call-dyn-eval-iter-new-dir x y z comms 3 1))
        ([string=? (car elem) "RRT"] ;row right #4
         (call-dyn-eval-iter-new-dir x y z comms 4 1))
        ([string=? (car elem) "PUP"] ;page up   #5
         (call-dyn-eval-iter-new-dir x y z comms 5 1))
        ([string=? (car elem) "PDW"] ;page down #6
         (call-dyn-eval-iter-new-dir x y z comms 6 1))
        ;PROG-MEM
        ([string=? (car elem) "NEA"] ;next elem A
         (travel-and-call x y z comms dir add-one-prog-mem-A-ptr))
        ([string=? (car elem) "PEA"] ;prev elem A
         (travel-and-call x y z comms dir sub-one-prog-mem-A-ptr))
        ([string=? (car elem) "NEB"] ;next elem B
         (travel-and-call x y z comms dir add-one-prog-mem-B-ptr))
        ([string=? (car elem) "PEB"] ;prev elem B
         (travel-and-call x y z comms dir sub-one-prog-mem-B-ptr))
        ([string=? (car elem) "SWP"] ;swap A <-> B   
         (travel-and-call x y z comms dir swap-prog-mem-ptr-A-B))
        ;CONTROL if #f skip next command
        ([string=? (car elem) "CAZ"] ;check if A zero
         (if (eq? get-prog-mem-val-at-A 0)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CBZ"] ;check if B zero
         (if (eq? get-prog-mem-val-at-B 0)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CAL"] ;check if A > B
         (if (> get-prog-mem-val-at-A get-prog-mem-val-at-B)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CBL"] ;check if B > A
         (if (> get-prog-mem-val-at-B get-prog-mem-val-at-A)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CIE"] ;check if A == B
         (if (eq? get-prog-mem-val-at-A get-prog-mem-val-at-B)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ;I/O
        ([string=? (car elem) "OIA"]
         (travel-and-call x y z comms dir  display-val-int-at-A))
        ([string=? (car elem) "OIB"]
         (travel-and-call x y z comms dir  display-val-int-at-B))
        ([string=? (car elem) "IIA"]
         (travel-and-call x y z comms dir  input-val-int-at-A))
        ([string=? (car elem) "IIB"]
         (travel-and-call x y z comms dir  input-val-int-at-B))
        ;MATH
        ;OTHER
        ([string=? (car elem) "NOP"] (display 501)); noop debug 501, treba nastaviti dalje
        ;ERROR HANDLER :)
        (else #f) 
        )))
  
  (dyn-eval-iter 0 0 0 comm-lst-ddd 4)) 






;testiranje
;(dyn-eval-xyz comm-lst-ddd-test)






        