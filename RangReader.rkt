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
      (else (get-list-of-commands (append lst (list (list (string->symbol cmd))))))))) 

;OK
(define (form-page-from-lists comm-lst)
  (let ((crnt-comm-lst (get-list-of-commands '())))
    (cond
      ([eq? (car crnt-comm-lst) #\S]
       (form-page-from-lists
        (append comm-lst (list (append (cdr crnt-comm-lst) (list '(STP)))))))
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
                           
                           ;prog-comm-tb-dir-ops
                           ;prog-comm-tb-single-ops
                           ;prog-comm-tb-cntrl-ops
                           
                           (list com-s-d)
                           (list page-start)

                           ;'((display com-lst))
                           
                           '((dyn-eval-xyz com-lst pg-strt))
                           )))


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

                 
  ;Prog-noop
  (define noop-cntr 0)
  (define (prog-noop) (set! noop-cntr (+ noop-cntr 1)))
                 
 ;Prog-check (skip)
  (define (prog-chck-A-z)
    (if (equal? (get-prog-mem-val-at-A) 0) 1 2))
                 
  (define (prog-chck-B-z)
    (if (eq? (get-prog-mem-val-at-B) 0) 1 2))
                 
  (define (prog-chck-A-l)
    (if (> (get-prog-mem-val-at-A) (get-prog-mem-val-at-B)) 1 2))
                 
  (define (prog-chck-B-l)
    (if (> (get-prog-mem-val-at-B) (get-prog-mem-val-at-A)) 1 2))
    
  (define (prog-chck-AB-eq)
    (if (eq? (get-prog-mem-val-at-A) (get-prog-mem-val-at-B)) 1 2))

  ;zaustavljanje programa - nije radilo kako treba
  ;(define (prog-row-halt) #t (exit))
  ;(define (prog-rndm-halt) #t (error "0"))

  ;možda nece trebat
  ;(define ns-es (make-base-namespace)) ne


                 ;PROG COM TABLES

  (define comm-ch-dir-ops-hsh-tb (hash
                                  'RUP 'prog-dir-Rw-Up
                                  'RDW 'prog-dir-Rw-Dw
                                  'RLF 'prog-dir-Rw-Lf
                                  'RRT 'prog-dir-Rw-Rt
                                  'PUP 'prog-dir-Pg-Up
                                  'PDW 'prog-dir-Pg-Dw                                  
                                  ))
                 
  (define comm-single-ops-hsh-tb (hash
                                  ;'STP prog-row-halt
                                  ;'STO prog-rndm-halt
                                  
                                  'NEA add-one-prog-mem-A-ptr
                                  'PEA sub-one-prog-mem-A-ptr
                                  'NEB add-one-prog-mem-B-ptr
                                  'PEB sub-one-prog-mem-B-ptr
                                  'SWP swap-prog-mem-ptr-A-B
                                  'CPA copy-val-at-A-into-B
                                  'CPB copy-val-at-B-into-A
                                  
                                  'OIA display-val-int-at-A
                                  'OIB display-val-int-at-B
                                  'OAA display-val-ascii-at-A
                                  'OAB display-val-ascii-at-B
                                  'IIA input-val-int-at-A
                                  'IIB input-val-int-at-B
                                  
                                  'AOA incr-mem-by-one-at-ptr-A
                                  'AOB incr-mem-by-one-at-ptr-B
                                  'SOA decr-mem-by-one-at-ptr-A
                                  'SOB decr-mem-by-one-at-ptr-B
                                  'REA reset-mem-val-at-ptr-A
                                  'REB reset-mem-val-at-ptr-B
                                  'ADA add-val-to-A
                                  'ADB add-val-to-B
                                  'SBA sub-val-to-A
                                  'SBB sub-val-to-B

                                  'NOP prog-noop
                                  ))
                 
  (define comm-cntrl-ops-hsh-tb (hash
                                 'CAZ prog-chck-A-z
                                 'CBZ prog-chck-B-z
                                 'CAL prog-chck-A-l
                                 'CBL prog-chck-B-l
                                 'CIE prog-chck-AB-eq
                                 ))
                 
                 
  ;glavna funkcija za interpretaciju 
  (define (dyn-eval-iter x y z comms dir)
    
    ;pozovi dyn-eval-iter sa novim smjerom
    (define (call-dyn-eval-iter-new-dir x y z comms dir step)
      (cond
        ([eq? dir 'prog-dir-Rw-Up] (dyn-eval-iter x (- y step) z comms dir)) ;row up
        ([eq? dir 'prog-dir-Rw-Dw] (dyn-eval-iter x (+ y step) z comms dir)) ;row dw
        ([eq? dir 'prog-dir-Rw-Lf] (dyn-eval-iter (- x step) y z comms dir)) ;row lf
        ([eq? dir 'prog-dir-Rw-Rt] (dyn-eval-iter (+ x step) y z comms dir)) ;row rt
        ([eq? dir 'prog-dir-Pg-Up] (dyn-eval-iter x y (+ z step) comms dir)) ;page up
        ([eq? dir 'prog-dir-Pg-Dw] (dyn-eval-iter x y (- z step) comms dir)) ;page dw
        (else (display dir)))) ;nebi nikad trebalo doci do tuda

    ;nastavi dalje ali za broj koraka koji vrati if
    (define (travel-by-proc x y z comms dir proc)
      (call-dyn-eval-iter-new-dir x y z comms dir (proc)))

    ;odradi proc i nastavi dalje
    (define (travel-and-call x y z comms dir proc step)
      (proc) (call-dyn-eval-iter-new-dir x y z comms dir step))

    ;interpretiraj naredbu
    (let ((elem (car (get-comm-lst-elem x y z)))) ;uzmi naredbu iz polja
      (cond
        ((hash-has-key? comm-single-ops-hsh-tb elem) ;obicne operacije
         (travel-and-call x y z comms dir (hash-ref comm-single-ops-hsh-tb elem) 1))
        
        ((hash-has-key? comm-ch-dir-ops-hsh-tb elem) ;change dir
         (call-dyn-eval-iter-new-dir x y z comms (hash-ref comm-ch-dir-ops-hsh-tb elem) 1))

        ((hash-has-key? comm-cntrl-ops-hsh-tb elem) ;za obradu proc koje vracaju broj koraka
         (travel-by-proc x y z comms elem (hash-ref comm-cntrl-ops-hsh-tb elem)))

        ((eq? elem 'STP) #t)

        ((eq? elem 'STO) #t)
        
        (else (displayln 'ERROR_Valjda) (display elem)) ;error handler        
        )))
  
 (dyn-eval-iter 0 0 start-page comm-lst-ddd 'prog-dir-Rw-Rt) ;pocetni poziv
 )))



  
  
 