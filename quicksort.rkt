#lang racket

#|Using the last index as the pivot, find all the numbers that
are smaller and put them infront of the pivot and then find all
the numbers that are larger and put them behind the pivot.

Verbose to clearly seperate out the steps
|#

(define lst '(10 9 4 1 11 7))

(define (median-of-three lst)  ;;can be used to guess the best pivot for a list
  (list-ref (positions-pivot (list (car lst)
                             (round (/ (length lst) 2))
                             (list-ref lst (sub1 (length lst))))) 1))


(define (drop-last lst) ;;or you can use whatever is in the last index as the pivot,
  (if (null? (cdr lst)) ;;but you need to drop it from the working list
             null
             (cons (car lst)(drop-last (cdr lst)))))


 (define (infront-pivot lst) ;;makes the before-pivot list
   (let([pivot (list-ref lst (sub1 (length lst)))])
     (letrec ([f(lambda (no-pivot-lst)
                  (if (null? no-pivot-lst)
                      null
                      (if  (<= (car no-pivot-lst) pivot)
                            (cons (car no-pivot-lst)(f (cdr no-pivot-lst)))
                            (f (cdr no-pivot-lst)))))])
      (f (drop-last lst)))))

 (define (behind-pivot lst) ;;makes the after-pivot list
   (let([pivot (list-ref lst (sub1 (length lst)))])
     (letrec ([f(lambda (no-pivot-lst)
                  (if (null? no-pivot-lst)
                      null
                      (if  (>= (car no-pivot-lst) pivot)
                            (cons (car no-pivot-lst)(f (cdr no-pivot-lst)))
                            (f (cdr no-pivot-lst)))))])
      (f (drop-last lst)))))

(define (positions-pivot lst) ;;makes one list with the pivot in the right spot, but infront and behind are unordered
  (append (append (infront-pivot lst) (list (list-ref lst (sub1 (length lst))))) (behind-pivot lst)))





(define (loop lst) ;;not recursive, not working
                   ;;base cases should be when there's one item left in the list.
  (append (append (positions-pivot (infront-pivot lst)) (list (list-ref lst (sub1 (length lst)))))
          (positions-pivot (behind-pivot lst))))
