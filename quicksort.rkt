#lang racket

#|From Chapter 4, Quicksort

To sort a list:
               1 Choose a pivot (usually the last index but in the code here it's the first.)
               2 Make two lists, all the numbers that are higher than the pivot and all the
                 numbers that are lower than the pivot.
               3 Put the pivot between the two unsorted lists.
               4 Start back at Step 1 (recursion), but now use the list that's infront of the pivot.
               5 The base case is an empty list or a list with one index (both meet the
                 definition of "in the right order.")
               6 Then do the same with the unordered list behind the pivot. Start back at Step 1.
               7 Append the lower list to the pivot and to the higher list.
|#

(define lst (list 9 5 7 3 5 3 3))

;;Save time with filter
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
#|
(define lst (list 9 5 7 3 5 3 3))

CALL:
(my-quicksort lst)

RESULT:
'(3 3 3 5 5 7 9)
|#