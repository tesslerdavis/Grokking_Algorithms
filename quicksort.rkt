#lang racket

;;Save time with filter

(define lst (list 9 5 7 3 5 3 3))

(define (quicksort lst)
  (if (null? lst)
      lst
      (let ([pivot (car lst)])
        (let ([to-sort (cdr lst)])
          (let ([lowr (filter (lambda (x) (> pivot x)) to-sort)])
            (let ([highr (filter (lambda (x) (<= pivot x)) to-sort)])
              (append (quicksort lowr) (cons pivot '()) (quicksort highr))))))))



;;Spend time without filter

(define (my-quicksort lst)
  (letrec ([f(lambda (lst pivot)
  (if (null? lst)
      lst
      (let ([pivot (car lst)])
        (let ([to-sort (cdr lst)])
          (let ([lowr (infront-pivot to-sort pivot)])
            (let ([highr (behind-pivot to-sort pivot)])
              (append (f lowr pivot) (cons pivot '()) (f highr pivot))))))))])
    (f lst 0)))
          

(define (infront-pivot lst pivot) ;;builds just the before-pivot list
  (if (null? lst)
      null
      (if  (<= (car lst) pivot)
           (cons (car lst)(infront-pivot (cdr lst) pivot))
           (infront-pivot (cdr lst) pivot))))


(define (behind-pivot lst pivot) ;;builds just the after-pivot list
  (if (null? lst)
      null
      (if  (> (car lst) pivot)
           (cons (car lst)(behind-pivot (cdr lst) pivot))
           (behind-pivot (cdr lst) pivot))))


(define (median-of-three lst)  ;;guesses the best pivot for any given list
  (list-ref (my-quicksort (list (car lst)
                      (round (/ (length lst) 2))
                      (list-ref lst (sub1 (length lst))))) 1))

