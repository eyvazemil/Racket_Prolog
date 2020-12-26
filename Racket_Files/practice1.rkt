#lang racket

; Cut list in two halves
(define (sizeList list)
  (if (null? list)
      0
      (+ 1 (sizeList (cdr list)))
  )
)

(define (cutList list pos)
  (if (> pos 0)
      (cons (car list) (cutList (cdr list) (- pos 1)))
      null
  )
)

(define (takeSecondHalf list half_list)
  (if (null? half_list)
      list
      (takeSecondHalf (cdr list) (cdr half_list))
  )
)

(define (myHalve _list)
  (if (null? _list)
      null
      (let ((size (sizeList _list)))
        (let ((firstHalf (cutList _list (/ size 2))))
          (if (equal? size 1)
              _list
              (list firstHalf (takeSecondHalf _list firstHalf))
          )
        )
      )
  )
)

; Multiply elements of list on the same indices
(define (myMulti list1 list2)
  (if (null? list1)
      list2
      (if (null? list2)
          list1
          (cons (* (car list1) (car list2)) (myMulti (cdr list1) (cdr list2)))
      )
  )
)

; Merge two lists in ascending order for the elements at the same indices
(define (minVal a b)
  (if (< a b) a b)
)

(define (maxVal a b)
  (if (> a b) a b)
)

(define (myMerge list1 list2)
  (if (null? list1)
      list2
      (if (null? list2)
          list1
          (let ((min (minVal (car list1) (car list2))))
            (let ((max (maxVal (car list1) (car list2))))
              (cons min (cons max (myMerge (cdr list1) (cdr list2))))
            )
          )
      )
  )
)

; Sum of lists
(define (sumOfLists list1 list2)
  (if (or (null? list1) (null? list2))
      null
      (cons (+ (car list1) (car list2)) (sumOfLists (cdr list1) (cdr list2)))
  )
)

(define (atom? val)
  (and (not (null? val))
       (not (pair? val))
  )
)

(define (AppendForSum list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (AppendForSum (cdr list1) list2))
  )
)

(define (flattenList lst)
  (if (null? lst)
      null
      (if (atom? (car lst))
          (cons (car lst) (flattenList (cdr lst)))
          (AppendForSum (flattenList (car lst)) (flattenList (cdr lst)))
      )
  )
)

(define (sumOfLists1 list1 list2)
  (sumOfLists (flattenList list1) (flattenList list2))
)

; Roll the list
(define (take_N_FirstElements list n)
  (if (or (null? list) (equal? n 0))
      null
      (cons (car list) (take_N_FirstElements (cdr list) (- n 1)))
  )
)

(define (incrementListByGivenValue list val)
  (if (null? list)
      null
      (if (equal? val 0)
          (cons (car list) (incrementListByGivenValue (cdr list) val))
          (incrementListByGivenValue (cdr list) (- val 1))
      )
  )
)

(define (Append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (Append (cdr list1) list2))
  )
)

(define (myRol list n)
  (Append (incrementListByGivenValue list n) (take_N_FirstElements list n))
)

; Recursive function
(define (rec n)
  (if (equal? n 0)
      1
      (if (equal? n 1)
          2
          (+ n (+ 2 (* 3 (rec (- n 2)))))
      )
  )
)

(define (rec1Helper n)
  (if (< n 0)
      null
      (cons (rec n) (rec1Helper (- n 1)))
  )
)

(define (reverseList list new_list)
  (if (null? list)
      new_list
      (reverseList (cdr list) (cons (car list) new_list))
  )
)

(define (rec1 n)
  (reverseList (rec1Helper n) null)
)

; Find average of values in the list
(define (sumOfElements lst)
  (if (null? lst)
      0
      (+ (car lst) (sumOfElements (cdr lst)))
  )
)

(define (numOfElements lst counter)
  (if (null? lst)
      0
      (+ 1 (numOfElements (cdr lst) (+ counter 1)))
  )
)

(define (averageList lst)
  (if (null? lst)
      null
      (/ (sumOfElements lst) (numOfElements lst 0))
  )
)

(define (averageAtom? x)
  (and (not (null? x))
       (not (pair? x))
  )
)

(define (averageAppendList list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (averageAppendList (cdr list1) list2))
  )
)

(define (averageFlattenList lst)
  (if (null? lst)
      null
      (if (averageAtom? (car lst))
          (cons (car lst) (averageFlattenList (cdr lst)))
          (averageAppendList (averageFlattenList (car lst)) (averageFlattenList (cdr lst)))
      )
  )
)

(define (averageList1 lst)
  (averageList (averageFlattenList lst))
)

; Sum of 2D list
(define (atomOf2DList? x)
  (and (not (null? x))
       (not (pair? x))
  )
)

(define (sumOf2DListHelper x)
  (if (null? x)
      0
      (+ (car x) (sumOf2DListHelper (cdr x)))
  )
)

(define (sumOf2DList list)
  (if (and (null? (cdr list)) (null? list))
      null
      (if (atomOf2DList? (car list))
          (if (and (not (null? (cdr list))) (null? (car (cdr list))))
              (cons (car list) (cons 0 (sumOf2DList (cdr (cdr list)))))
              (cons (car list) (sumOf2DList (cdr list)))
          )
          (cons (sumOf2DListHelper (car list)) (sumOf2DList (cdr list)))
      )
  )
)