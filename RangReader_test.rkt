#lang racket
(require syntax/datum)

(define comm-set-ddd '())

(define com-ss-dd '())

(define (read-syntax path port)
  ;(define
  (define port-str (port->string port))
  
  (define ni-st (open-input-string port-str))
  (define (get-char-from-port) (read-char ni-st))
  
  
  (define start-y 0)

  (define (update-start-y x)
    (set! start-y x))

;(define (get-char-from-port) (set! test (cdr test)) (car test))

;OK
(define (get-next-legal-char)  
  (let ((crnt-char (get-char-from-port)))
    (cond
      ([eof-object? crnt-char] (get-next-legal-char))
      ([eq? #\newline crnt-char] (get-next-legal-char))
      ;([eq? #\? crnt-char] (get-next-legal-char))
      ([char-whitespace? crnt-char] (get-next-legal-char))
      (else crnt-char))))

;OK
(define (get-next-command)
  (let ((chrA (get-next-legal-char))
        (chrB (get-next-legal-char))
        (chrC (get-next-legal-char)))
    (string chrA chrB chrC)))

;OK
(define (get-list-of-commands lst)
  (let ((cmd (get-next-command)))
    (cond
      ([string=? cmd "STR"] (cons #\- lst)) ;main page
      ([string=? cmd "STP"] (cons #\S lst)) ;end of line
      ([string=? cmd "NEW"] (cons #\# lst)) ;new page
      ([string=? cmd "END"] (cons #\+ lst)) ;end of file
      (else (get-list-of-commands (append lst (list (list cmd)))))))) 

;OK
(define (form-page-from-lists comm-lst)
  (let ((crnt-comm-lst (get-list-of-commands '())))
    (cond
      ([eq? (car crnt-comm-lst) #\S]
       (form-page-from-lists
        (append comm-lst (list (append (cdr crnt-comm-lst) (list '("STP")))))))
      ([eq? (car crnt-comm-lst) #\#] (cons #\# comm-lst))
      ([eq? (car crnt-comm-lst) #\-] (cons #\- comm-lst))
      ([eq? (car crnt-comm-lst) #\+] (cons #\+ comm-lst))
      (else 234))));nebi trebalo

;OK
(define (form-list-of-pages pgs-lst cntr)
  (let ((crnt-pg (form-page-from-lists '())))
    (cond
      ([eq? (cdr crnt-pg) null]
       (form-list-of-pages pgs-lst cntr))
      ([eq? (car crnt-pg) #\#]
       (form-list-of-pages (append pgs-lst (list (cdr crnt-pg))) (+ cntr 1)))
      ([eq? (car crnt-pg) #\-]
       (update-start-y (+ cntr 1))
       (form-list-of-pages (append pgs-lst (list (cdr crnt-pg))) (+ cntr 1)))
      ([eq? (car crnt-pg) #\+] (append pgs-lst (list (cdr crnt-pg))))
      (else 555))));nebi trebalo
  
  ;(datum->syntax #f (append '(module lucy racket (display 1)))))

  (define com-s-d
    (with-datum ([(a ...) (list (form-list-of-pages '() 0))])
      (datum (define xxx 'a ...))))

  
  ;(set! comm-set-ddd (form-list-of-pages '() 0))
  ;(set! com-ss-dd (append '(define comm-lst-ddd-test) (list comm-set-ddd)))
  ;(display com-s-d)
  (datum->syntax #f (append '(module idfk racket)
                           prog-mem-def
                           cmdd
                           coms
                           
                           (list com-s-d)
                           ;'((displayln xxx))
                           ;'((displayln " "))
                           ;'((displayln comm-lst-ddd-test))
                           ;'((equal? xxx comm-lst-ddd-test))
                           ;'((displayln " "))
                           ;'((car (car (car xxx))))
                           ;'((car (car (car comm-lst-ddd-test))))
                           ;'((displayln " "))
                           '((dyn-eval-xyz xxx))
                           )))

;(datum->syntax #f (append '(module lucy racket) coms prog-mem-def cmdd '((dyn-eval-xyz comm-lst-ddd-test)))))
 

(provide read-syntax)




(define coms '((define comm-lst-ddd-test
  '(   ;glavno polje
    (  ;z0
     ((IIA) (OIA) (RDW) (STP))        ;y0
     ((NOP) (PUP) (RLF) (STP))) ;y1
    (  ;z1
     ((NOP) (NOP) (NOP) (STP))        ;y0
     ((NOP) (RRT) (OIA) (STP))) ;y1
    )) ;x0   ;x1   ;x2   ;x3
  ))
     


;memorija
(define prog-mem-def '((define prog-mem-lst '(0 0 0 0 0 0)) 
(define prog-mem-A-ptr 2)
(define prog-mem-B-ptr 2))) 


(define cmdd '((define (dyn-eval-xyz comm-lst-ddd)

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
        (else (if (string=? (car elem) "IIA")  elem (car elem))) 
        )))
  
  (dyn-eval-iter 0 0 0 comm-lst-ddd 4))))

;(dyn-eval-xyz comm-lst-ddd-test)
  