#lang racket

#|From Chapter 8, Greedy Algorithms

For this type, at each step take the locally optimal solution and by the end you've reached the
globally optimal solution. Sometimes greedy algorithms don't give the best solutions,
but they do tend to give good ones.

The example in the book is about scheduling classes to get the most use out of a classroom.

At each step, choose the one that finishes earliest.
|#

;;creates "class" datatype
(struct class (name starts ends)  #:transparent)

;;lists needs to be ordered by start time
(define classes (list
(class "gym" 8.5 9)
(class "art" 9 10)
(class "English" 9.5 10.5)
(class "math" 10 11)
(class "cs" 10.5 11.5)
(class "music" 11 12)
))


(define (greedy-scheduler lst)
  (let ([first-class (car lst)]
        [current-final (car lst)])
    (letrec ([f(lambda (lst)
                 (cond [(null? lst) null]
                       [(= (class-starts first-class) (class-starts (car lst)))
                        (cons current-final (f (cdr lst)))]
                       [(>= (class-starts (car lst)) (class-ends current-final))
                        (begin
                          (set! current-final (car lst))
                          (cons current-final (f (cdr lst))))]
                       [#t (f (cdr lst))]))])
      (f lst))))