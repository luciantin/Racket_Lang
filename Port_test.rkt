#lang racket
;(define test (append '(#\a #\b #\space #\4 #\5 #\newline ) (list eof ) ))
  
(define test-a "###
  HJH FKD STP
---
ABC DFG ACD STP
HGF DSD JHG STP
###
VHJ FGH STP
DDD DSD   STP
+++
")

(define ni-st (open-input-string test-a))

(define (get-char-from-port) (read-char ni-st))

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
      ([string=? cmd "---"] (cons #\- lst)) ;main page
      ([string=? cmd "STP"] (cons #\S lst)) ;end of line
      ([string=? cmd "###"] (cons #\# lst)) ;new page
      ([string=? cmd "+++"] (cons #\+ lst)) ;end of file
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






;(form-com-from-port '() 0)


