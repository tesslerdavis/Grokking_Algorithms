#lang racket

;make and populated hash table
(define ht (make-hash))
(hash-set! ht "me" '("alice" "bob" "claire"))
(hash-set! ht "alice" '("peggy"))
(hash-set! ht "peggy" '())
(hash-set! ht "bob" '("jim"))
(hash-set! ht "jim" '())
(hash-set! ht "claire" '())


;;goes through first-nod's neighbors before moving to friends of friends.
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

#|
(by-degrees ht "me" "bob")
"exists inside friends network"
> (by-degrees ht "me" "jim")
"exists inside friends network"
> (by-degrees ht "me" "james")
"does not exist or outside friends network"
> (by-degrees ht "me" "mike")
"does not exist or outside friends network"
|#