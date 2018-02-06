#lang racket

;makes and populates hash table
(define ht (make-hash))
(hash-set! ht "me" '("alice" "bob" "claire"))
(hash-set! ht "alice" '("peggy"))
(hash-set! ht "peggy" '())
(hash-set! ht "bob" '("jim"))
(hash-set! ht "jim" '())
(hash-set! ht "claire" '())


;;skips repeats
(define (already-done name name-lst)
  (if (null? name-lst)
      #f
      (if (equal? name (car name-lst))
          #t
          (already-done name (cdr name-lst)))))

;;goes through first-node's neighbors before moving to friends of friends.
(define (by-degrees hashname first-node node-to-find)
  (let ([que (hash-ref hashname first-node)]
        [checked '()])
    (letrec ([f(lambda (que)
                 (cond [(null? que)
                        "Boo!"]
                       [(already-done (car que) checked)(f (cdr que))] ;;checks for repeats
                       [(equal? (car que) node-to-find)
                        "Bingo!"]
                       [#t (begin
                             (set! que (append que (hash-ref hashname (car que)))) ;;failed matches add their friends to the que
                             (set! checked (append (list(car que)) checked))
                             (f (cdr que)))]))])
      (f que))))

#|
(by-degrees ht "me" "bob")
"Bingo!"
> (by-degrees ht "me" "jim")
"Bingo!"
> (by-degrees ht "me" "james")
"Boo!"
> (by-degrees ht "me" "mike")
"Boo!"
|#