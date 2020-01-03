#lang racket
;(define test (append '(#\a #\b #\space #\4 #\5 #\newline ) (list eof ) ))
  
(define test-a "NEW
  HJH FKD STP
NEW
ABC DFG ACD STP
HGF DSD JHG STP
STR
VHJ FGH STP
DDD DSD   STP
END
")

(define ni-st (open-input-string test-a))

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
       (form-page-from-lists (append comm-lst (list (cdr crnt-comm-lst)))))
      ([eq? (car crnt-comm-lst) #\#] (cons #\# comm-lst))
      ([eq? (car crnt-comm-lst) #\-] (cons #\- comm-lst))
      ([eq? (car crnt-comm-lst) #\+] (cons #\+ comm-lst))
      (else (display 234)))))

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
      (else 555))))
       




(form-list-of-pages '() 0)


