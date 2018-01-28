#lang racket

;makes hash table
(define ht (make-hash))

;adds to it
(hash-set! ht "me" '("alice" "bob" "claire"))
(hash-set! ht "alice" '("peggy"))
(hash-set! ht "peggy" '())
(hash-set! ht "bob" '("jim"))
(hash-set! ht "claire" '())


;;takes a hash table, start nod, and search nod.
;;goes through nod neighbors before moving to friends of friends.

(define (by-degrees hashname first-nod nod-to-find)
  (let ([que (hash-ref hashname first-nod)])
    (letrec ([f(lambda (que)
                 (cond [(null? que)
                        "does not exist or outside friends network"]
                       [(equal? (car que) nod-to-find)
                        "exists inside friends network"]
                       [#t (begin
                             (set! que (append que (hash-ref hashname (car que)))) ;;failed matches add their friends to the que
                                          (f (cdr que)))]))])
(f que))))
