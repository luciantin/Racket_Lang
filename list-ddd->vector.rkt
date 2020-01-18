#lang racket

(define tst-lst

(list
 (list
  (list 1 2 3)
  (list 4 5 6))
 (list
  (list 7 8 9))) )

(map (lambda (lst) (list->vector lst)) '(( 1 2 3) (4 5 6)))

(vector-ref
 (vector-ref
  (list->vector
   (map (lambda (lst) (list->vector lst)) '(( 1 2 3) (4 5 6))))
  1)
 1)

;(define (lst->vec lst) (


(define (list-ddd->vector lst-ddd)
  (list->vector (map (lambda (lst) (list->vector (map list->vector lst))) lst-ddd)))


(define tst-vctr (list-ddd->vector tst-lst))

(vector-ref (vector-ref (vector-ref tst-vctr 0)0)1)

(define prog-lst-tst '(
                       (
                        ((STO) (STP))
                        ((RRT) (AOA) (AOA) (AOA) (AOA) (AOA) (AOA) (AOA) (AOA) (AOA) (AOA) (RDW) (STP))
                        ((PUP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (NOP) (CPA) (PEB) (RLF) (STP)))
                       (
                        ((STO) (STP))
                        ((PDW) (STP))
                        ((PUP) (STP)))
                       (
                        ((NEA) (REA) (PEB) (PEB) (AOB) (RDW) (STP))
                        ((PDW) (NOP) (NEB) (NEB) (NOP) (RLF) (STP))
                        ((RRT) (OIA) (STP)))
                       ))


(list-ddd->vector prog-lst-tst)


(define (map-lst-to-vect lst) (map list->vector lst))

(define (prog-list-dddd->vector prog-lst)
  (list->vector
   (map (lambda (lst)
          (list->vector (map (lambda (lsa) (list->vector  lsa)) lst)))
        prog-lst)))



(define x (prog-list-dddd->vector prog-lst-tst))



(newline)
(vector-ref (vector-ref (vector-ref x 0)0)0) ;single comm ref
(newline)
(vector-ref (vector-ref x 0) 0) ;row ref
(newline)
(vector-ref x 0) ;pg ref


































