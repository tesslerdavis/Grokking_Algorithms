#lang racket

;super very basic proof of concept that it's possible to make a hash table
;;and search through lists assigned to keys by degree of seperation.

;makes hash table
(define ht (make-hash))

;adds to it
(hash-set! ht "me" '("alice" "bob" "claire"))
(hash-set! ht "alice" '("peggy" "jon" "fred"))

;;takes a hash table, start nod, and search nod.
;;goes through nod neighbors before moving to friends of friends.
(define (by-degrees hashname first-nod value-to-find)
  (let ([nod-neighbors(hash-ref hashname first-nod)])
    (letrec ([f(lambda (lst)
                (cond [(null? lst) (by-degrees hashname (car nod-neighbors) value-to-find)] 
                      [(equal? (car lst) value-to-find) "found a match"]
                      [#t (f (cdr lst))]))])
           (f nod-neighbors))))


#|TO DO
 - (much later) use lists of pairs with bool flags to skip repeats
 - have an actual queue
 - fix how the null? branch only allows for a two-key hash because it can't move down the nod-neighbors
   list (which having a real queue will solve.
|#