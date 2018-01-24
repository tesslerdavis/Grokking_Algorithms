#lang racket

#|from Chapter Two, Selection Sort

The example is about sorting a playlist. Selection sort finds
the most-played song and adds it to a new list. Then it goes back
to the original list and finds the most-play song out of the
remaining list items, and adds it to the second list. Each time,
it has to sort all the way through the original (but now shortening)
list to find the latest most-played.

BigO => O(n X n)
|#

(define lst '(10 20 300 300 30 1 3 3 100))

;; finds the highest number in a list
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


;;removes the highest number from a list, but leaves repeats
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


;;builds a new list going from highest to lowest, with repeats
(define (highests->lowests lst)
(letrec ([f (lambda (lst)
              (if (null? lst)
              null
              (cons (find-highest lst)(f (without-highest (find-highest lst) lst)))))])
  (f lst)))

#|CALL
(highests->lowests lst)

RESULT
'(300 300 100 30 20 10 3 3 1)
|#