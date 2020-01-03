#lang racket
;#lang br/quicklang

(define (read-syntax path port)
  (define (get-char-from-port) (read-char port))    
  (datum->syntax #f (append '(module lucy racket) (list '(display 12)))))
 

(provide read-syntax)
 