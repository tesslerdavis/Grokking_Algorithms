#lang racket

#|Save time with filter|#

(define (quicksort lst)
  (if (null? lst)
      lst
  (let ([pivot (car lst)])
        (let ([to-sort (cdr lst)])
          (let ([lowr (filter (lambda (x) (> pivot x)) to-sort)])
          (let ([highr (filter (lambda (x) (<= pivot x)) to-sort)])
            (append (quicksort lowr) (cons pivot '()) (quicksort highr))))))))
          
          
#|First steps of quicksort: using the last index as the pivot, find all the numbers that
are smaller and put them infront of the pivot and then find all
the numbers that are larger and put them behind the pivot.|#


(define (median-of-three lst)  ;;can be used to guess the best pivot for a list
  (list-ref (pivoted-lst-maker (list (car lst)
                             (round (/ (length lst) 2))
                             (list-ref lst (sub1 (length lst))))) 1))


(define (drop-last lst) ;;or you can use whatever is in the last index as the pivot,
  (if (null? (cdr lst)) ;;but you need to drop it from the working list,
                        ;;or just check for no cdr (one item left) as base case in pivoted-lst-maker  
             null
             (cons (car lst)(drop-last (cdr lst)))))



(define lst '(6 9 10 5 7))


 (define (pivoted-lst-maker lst) ;;makes the whole list with elements on the correct side of the pivot, but unordered
   (let([pivot (list-ref lst (sub1 (length lst)))])
     (letrec ([f(lambda (no-pivot-lst)
                  (if (null? no-pivot-lst)
                      (cons pivot null)
                      (if  (<= (car no-pivot-lst) pivot)
                            (cons (car no-pivot-lst)(f (cdr no-pivot-lst)))
                            (flatten (cons (f (cdr no-pivot-lst)) (car no-pivot-lst))))))])
      (f (drop-last lst))))) ;;if the base case is (null? (cdr lst)), there's no need for (drop-last lst) 



;;Combined the three here into the pivoted-lst-maker using better if branches and (flatten '(1 2 3)) to make
;;one smooth list

 (define (infront-pivot lst) ;;makes just the before-pivot list
   (let([pivot (list-ref lst (sub1 (length lst)))])
     (letrec ([f(lambda (no-pivot-lst)
                  (if (null? no-pivot-lst)
                      null
                      (if  (<= (car no-pivot-lst) pivot)
                           (cons (car no-pivot-lst)(f (cdr no-pivot-lst)))
                           (f (cdr no-pivot-lst)))))])
       (f (drop-last lst)))))

 (define (behind-pivot lst) ;;makes just the after-pivot list
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





;;not recursive, not working
#|(define (quicksort lst)
  (if (null? (cdr lst)
             null
             (cons 



(infront-pivot (infront-pivot  (infront-pivot lst)))
'(1)|#