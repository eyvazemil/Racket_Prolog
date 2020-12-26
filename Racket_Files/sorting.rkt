#lang racket

; Select Sort
(define (minList list min counter)
  (if (null? list)
      min
      (if (or (equal? counter 0) (< (car list) min))
          (minList (cdr list) (car list) (+ counter 1))
          (minList (cdr list) min (+ counter 1))
      )
  )
)

(define (changeAtPos list pos val counter)
  (if (null? list)
      null
      (if (equal? counter pos)
          (cons val (changeAtPos (cdr list) pos val (+ counter 1)))
          (cons (car list) (changeAtPos (cdr list) pos val (+ counter 1)))
      )
  )
)

(define (findAtPos list val counter)
  (if (null? list)
      0
      (if (equal? (car list) val)
          counter
          (findAtPos (cdr list) val (+ counter 1))
      )
  )
)

(define (selectSort list)
  (if (null? list)
      null
      (if (equal? (car list) (minList list 0 0))
          (cons (car list) (selectSort (cdr list)))
          (cons (minList list 0 0) (selectSort (changeAtPos (cdr list) (findAtPos (cdr list) (minList (cdr list) 0 0) 0) (car list) 0)))
      )
  )
)

; Insert Sort
(define (insertIntoList list val pos counter)
  (if (null? list)
      (cons val null)
      (if (and (> val (car list)) (< counter pos))
          (cons (car list) (insertIntoList (cdr list) val pos (+ counter 1)))
          (cons val list)
      )
  )
)

(define (removeByPos list pos counter)
  (if (null? list)
      null
      (if (equal? counter pos)
          (cdr list)
          (cons (car list) (removeByPos (cdr list) pos (+ counter 1)))
      )
  )
)

(define (insertSortHelper list list_tmp counter)
  (if (null? list)
      list_tmp
      (insertSortHelper (cdr list) (insertIntoList (removeByPos list_tmp counter 0) (car list) counter 0) (+ counter 1))
  )
)

(define (insertSort list)
  (insertSortHelper list list 0)
)

(define (sumInRange list min max)
  (if (null? list)
      0
      (if (and (>= (car list) min) (<= (car list) max))
          (+ (car list) (sumInRange (cdr list) min max))
          (sumInRange (cdr list) min max)
      )
  )
)