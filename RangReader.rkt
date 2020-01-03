#lang racket
;#lang br/quicklang

(define (read-syntax path port)
  (define (get-char-from-port) (read-char port))
  
  (define (form-com-from-port comm cntr)
    (let ((crnt-char (get-char-from-port)))
      (cond
        ([eof-object? crnt-char] comm)
        ([char-whitespace? crnt-char] (form-com-from-port comm cntr))
        
    
  (datum->syntax #f (append '(module lucy racket))))
 

(provide read-syntax)
 