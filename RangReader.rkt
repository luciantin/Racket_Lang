#lang racket
(require syntax/datum)


(define (read-syntax path port)
  ;(define
  (define port-str (port->string port))
  
  (define ni-st (open-input-string port-str))
  (define (get-char-from-port) (read-char ni-st))
  
  
  (define start-y 0)

  (define (update-start-y x)
    (set! start-y x))

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
  
  
  (define com-s-d
    (with-datum ([(a ...) (list (form-list-of-pages '() 0))])
      (datum (define com-lst 'a ...))))

  (define page-start
   (with-datum ([(a ...) (list start-y)])
     (datum (define pg-strt a ...))))

  
  (datum->syntax #f (append '(module idfk racket)
                           prog-mem-def
                           cmdd
                           
                           (list com-s-d)
                           (list page-start)
                           
                           '((dyn-eval-xyz com-lst pg-strt)))))



(provide read-syntax)



;memorija
(define prog-mem-def '((define prog-mem-lst '(0 0 0 0 0 0)) 
(define prog-mem-A-ptr 2)
(define prog-mem-B-ptr 2))) 


(define cmdd '((define (dyn-eval-xyz comm-lst-ddd start-page)

  ;vrati elem na x y z poz
  (define (get-comm-lst-elem x y z)
    (list-ref (list-ref (list-ref comm-lst-ddd z) y) x))

  ;get vrijednost memorije
  (define (get-prog-mem-val-at-A)
    (list-ref prog-mem-lst prog-mem-A-ptr))
  (define (get-prog-mem-val-at-B)
    (list-ref prog-mem-lst prog-mem-B-ptr))
                 
  ;promjena vrijednosti  memorije
  (define (set-prog-mem-A! x)
    (set! prog-mem-lst (list-set prog-mem-lst prog-mem-A-ptr x)))
  (define (set-prog-mem-B! x)
    (set! prog-mem-lst (list-set prog-mem-lst prog-mem-B-ptr x)))

  ;promijena vrijednosti ptr A i B ... (adrese)            
  (define (change-prog-mem-A-ptr val proc)
    (set! prog-mem-A-ptr (proc prog-mem-A-ptr val)))
                 
  (define (change-prog-mem-B-ptr val proc)
    (set! prog-mem-B-ptr (proc prog-mem-B-ptr val)))

  (define (incr-prog-mem-A-ptr x) (change-prog-mem-A-ptr x +))
  (define (decr-prog-mem-A-ptr x) (change-prog-mem-A-ptr x -))

  (define (incr-prog-mem-B-ptr x) (change-prog-mem-B-ptr x +))
  (define (decr-prog-mem-B-ptr x) (change-prog-mem-B-ptr x -))
  
  (define (add-one-prog-mem-A-ptr) (incr-prog-mem-A-ptr 1))
  (define (sub-one-prog-mem-A-ptr) (decr-prog-mem-A-ptr 1))
  
  (define (add-one-prog-mem-B-ptr) (incr-prog-mem-B-ptr 1))
  (define (sub-one-prog-mem-B-ptr) (decr-prog-mem-B-ptr 1))

  ;swap               
  (define (swap-prog-mem-ptr-A-B)
    (let ((tmp get-prog-mem-val-at-A))
      (set-prog-mem-A! get-prog-mem-val-at-B)
      (set-prog-mem-B! tmp)))

   ;reset val              
  (define (reset-mem-val-at-ptr-A)
    (set-prog-mem-A! 0))

  (define (reset-mem-val-at-ptr-B)
    (set-prog-mem-B! 0))
                 
   ;incr val              
  (define (incr-mem-by-one-at-ptr-A)
    (set-prog-mem-A! (+ (get-prog-mem-val-at-A) 1)))

  (define (incr-mem-by-one-at-ptr-B)
    (set-prog-mem-B! (+ (get-prog-mem-val-at-B) 1)))

  ;decr val
  (define (decr-mem-by-one-at-ptr-A)
    (set-prog-mem-A! (- (get-prog-mem-val-at-A) 1)))

  (define (decr-mem-by-one-at-ptr-B)
    (set-prog-mem-B! (- (get-prog-mem-val-at-B) 1)))               
                 
  ;copy val             
  (define (copy-val-at-A-into-B)
    (set-prog-mem-B! (get-prog-mem-val-at-A)))
  
  (define (copy-val-at-B-into-A)
    (set-prog-mem-A! (get-prog-mem-val-at-B)))

  ;add
  (define (add-val-to-B)
    (set-prog-mem-B! (+ (get-prog-mem-val-at-A) (get-prog-mem-val-at-B))))
  
  (define (add-val-to-A)
    (set-prog-mem-A! (+ (get-prog-mem-val-at-A) (get-prog-mem-val-at-B))))

  ;sub
  (define (sub-val-to-A)
    (set-prog-mem-A! (- (get-prog-mem-val-at-A) (get-prog-mem-val-at-B))))
  
  (define (sub-val-to-B)
    (set-prog-mem-B! (- (get-prog-mem-val-at-A) (get-prog-mem-val-at-B))))
                 
  ;Display mem
  (define (display-val-int-at-A)
    (display (get-prog-mem-val-at-A)))

  (define (display-val-int-at-B)
    (display (get-prog-mem-val-at-B)))

  (define (display-val-ascii-at-A)
    (display (integer->char (get-prog-mem-val-at-A))))

  (define (display-val-ascii-at-B)
    (display (integer->char (get-prog-mem-val-at-B))))
                 

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
        ([string=? (car elem) "STP"] #t) ;stop prog && row
        ([string=? (car elem) "STO"] #t) ;stop prog
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
        ([string=? (car elem) "CPA"] ;cp A -> B
         (travel-and-call x y z comms dir copy-val-at-A-into-B))
        ([string=? (car elem) "CPB"] ;cp B -> A
         (travel-and-call x y z comms dir copy-val-at-B-into-A))
        ;CONTROL if #f skip next command
        ([string=? (car elem) "CAZ"] ;check if A zero
         (if (equal? (get-prog-mem-val-at-A) 0)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CBZ"] ;check if B zero
         (if (eq? (get-prog-mem-val-at-B) 0)
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CAL"] ;check if A > B
         (if (> (get-prog-mem-val-at-A) (get-prog-mem-val-at-B))
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CBL"] ;check if B > A
         (if (> (get-prog-mem-val-at-B) (get-prog-mem-val-at-A))
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ([string=? (car elem) "CIE"] ;check if A == B
         (if (eq? (get-prog-mem-val-at-A) (get-prog-mem-val-at-B))
             (call-dyn-eval-iter-new-dir x y z comms dir 1)
             (call-dyn-eval-iter-new-dir x y z comms dir 2)))
        ;I/O
        ([string=? (car elem) "OIA"] ;out int val at A
         (travel-and-call x y z comms dir  display-val-int-at-A))
        ([string=? (car elem) "OIB"] ;out int val at B
         (travel-and-call x y z comms dir  display-val-int-at-B))
        ([string=? (car elem) "OAA"] ;out int val at A
         (travel-and-call x y z comms dir  display-val-ascii-at-A))
        ([string=? (car elem) "OAB"] ;out int val at B
         (travel-and-call x y z comms dir  display-val-ascii-at-B))
        ([string=? (car elem) "IIA"] ;in int val in  A
         (travel-and-call x y z comms dir  input-val-int-at-A))
        ([string=? (car elem) "IIB"] ;in int val in  B
         (travel-and-call x y z comms dir  input-val-int-at-B))
        ;MATH
        ([string=? (car elem) "AOA"] ;add one to A
         (travel-and-call x y z comms dir  incr-mem-by-one-at-ptr-A))
        ([string=? (car elem) "AOB"] ;add one to B
         (travel-and-call x y z comms dir  incr-mem-by-one-at-ptr-B))
        ([string=? (car elem) "SOA"] ;sub one form A
         (travel-and-call x y z comms dir  decr-mem-by-one-at-ptr-A))
        ([string=? (car elem) "SOB"] ;sub one from B 
         (travel-and-call x y z comms dir  decr-mem-by-one-at-ptr-B))
        ([string=? (car elem) "REA"] ;reset val at A
         (travel-and-call x y z comms dir  reset-mem-val-at-ptr-A))
        ([string=? (car elem) "REB"] ;reset val at B
         (travel-and-call x y z comms dir  reset-mem-val-at-ptr-B))
        ([string=? (car elem) "ADA"] ;A + B save in A
         (travel-and-call x y z comms dir  add-val-to-A))
        ([string=? (car elem) "ADB"] ;A + B save in B
         (travel-and-call x y z comms dir  add-val-to-B))
        ([string=? (car elem) "SBA"] ;A - B save in A
         (travel-and-call x y z comms dir  sub-val-to-A))
        ([string=? (car elem) "SBB"] ;A - B save in B
         (travel-and-call x y z comms dir  sub-val-to-B))
        
        ;OTHER
        ([string=? (car elem) "NOP"]
         (call-dyn-eval-iter-new-dir x y z comms dir 1)); treba nastaviti dalje
        ;ERROR HANDLER
        (else (display (car elem))) 
        )))
  
  (dyn-eval-iter 0 0 start-page comm-lst-ddd 4))))
 