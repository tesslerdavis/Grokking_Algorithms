#lang racket

(define lst '(10 20 30 30 1 3 3))

(define (find-highest lst)
        (letrec ([highest (car lst)]
                 [ f (lambda (lst)
                       (if (or (null? lst) (equal? (cdr lst) '()))
                           highest
                           (if (>= highest (car (cdr lst)))
                                (f (cdr lst))
                                (begin
                                 (set! highest (car (cdr lst)))
                                 (f (cdr lst))))))])
          (f lst)))



(define (without-highest highest lst)
 (letrec ([include #f]
          [f (lambda (lst)
               (if (null? lst)
                    null
                    (if (and (equal? highest (car lst)) (equal? include #f))
                        (begin
                          (set! include #t)
                          (f (cdr lst)))
                        (cons (car lst)(f (cdr lst))))))])

   (f lst)))


(define (highests->lowests lst)
(letrec ([f (lambda (lst)
              (if (null? lst)
              null
              (cons (find-highest lst)(f (without-highest (find-highest lst) lst)))))])
  (f lst)))