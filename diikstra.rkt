#lang racket

(define my-graph (make-hash))
(hash-set! my-graph "start" '((cons "a" 6) (cons "b" 2)))
(hash-set! my-graph "a" '((cons "finish" 1)))
(hash-set! my-graph "b" '((cons "a" 3)(cons "finish" 5)))
(hash-set! my-graph "finish" '())


(define costs (make-hash)) ;;the weights of the edges to start node's neighbors.
(hash-set! costs "a" 6)
(hash-set! costs "b" 2)
(hash-set! costs "finish" +inf.0) ;;+inf.0 is Racket's infinity
                                  ;;there is no direct path from start to finish,
                                  ;;so set to inifinity

(define parents (make-hash))
(hash-set! parents "a" "start") ;;relationship is "parent of key is value" 
(hash-set! parents "b" "start")




;;currently goes through smallest nod once and makes all changes and then stops.
;;making the values into 
(define done-nods '())

(define smallest-cost
  (let ([vals-lst (hash->list costs)])
    (let ([smallest (car vals-lst)])
      (letrec ([f(lambda (lst)
                   (if (null? lst)
                       (car smallest)
                       (if (< (cdr (car lst)) (cdr smallest))
                           (begin
                             (set! smallest (car lst))
                             (f (cdr lst)))
                           (f (cdr lst)))))])
        (f vals-lst)))))

(define (proc-smallests-values x)
  (let ([neighbor-lst (hash-ref my-graph smallest-cost)])
    (letrec ([f(lambda (neighbor-lst)
                 (if (null? neighbor-lst)
                     costs
                     (let ([new-cost (+ (hash-ref costs smallest-cost)
                                        (car (cdr (cdr (car neighbor-lst)))))])
                       (if (> new-cost (car (cdr (cdr (car neighbor-lst)))))
                           (begin
                             (hash-set! costs (car (cdr (car neighbor-lst))) new-cost)
                             (hash-set! parents (car (cdr (car neighbor-lst)))
                                        smallest-cost) 
                             (f (cdr neighbor-lst)))
                           (f (cdr neighbor-lst))))))])
      (f neighbor-lst))))