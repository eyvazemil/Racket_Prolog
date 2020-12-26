#lang racket

; First tutorial

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))
  )
)

(define (recLen x)
  (if (null? x)
      0
      (if (atom? (car x))
          (+ 1 (recLen (cdr x)))
          (+ (recLen (car x)) (recLen (cdr x)))
      )
  )
)

(define (mySum x)
  (if (null? x)
      0
      (if (atom? (car x))
          (+ (car x) (mySum (cdr x)))
          (+ (mySum (car x)) (mySum (cdr x)))
      )
  )
)

(define (myHasElement x n)
  (if (null? x)
      false
      (if (atom? (car x))
          (if (equal? (car x) n)
              true
              (myHasElement (cdr x) n)
          )
          (myHasElement (car x) n)
      )
  )
)

(define (myNthElement x n found found_num)
  (if (equal? found true)
      found_num
      (if (null? x)
          false
          (if (atom? (car x))
              (if (equal? n 0)
                  (myNthElement (cdr x) (- n 1) true (car x))
                  (myNthElement (cdr x) (- n 1) false -1)
              )
              (or (myNthElement (car x) n false -1)
                  (myNthElement (cdr x) (- n (recLen (car x))) false -1)
              )
          )
      )
  )
)

(define (myMin a b)
  (if (< a b) a b)
)

(define (myMax a b)
  (if (> a b) a b)
)

(define (myMinL list current_min counter)
  (if (null? list)
      current_min
      (if (equal? counter 0)
          (myMinL (cdr list) (car list) (+ counter 1))
          (if (< (car list) current_min)
              (myMinL (cdr list) (car list) (+ counter 1))
              (myMinL (cdr list) current_min (+ counter 1))
          )
      )
  )
)

(define (myMaxL list current_max counter)
  (if (null? list)
      current_max
      (if (equal? counter 0)
          (myMaxL (cdr list) (car list) (+ counter 1))
          (if (> (car list) current_max)
              (myMaxL (cdr list) (car list) (+ counter 1))
              (myMaxL (cdr list) current_max (+ counter 1))
          )
      )
  )
)

(define (myFact n)
  (if (or (equal? n 0) (equal? n 1))
      1
      (* n (myFact (- n 1)))
  )
)

(define (myFib n)
  (if (equal? n 0)
      0
      (if (equal? n 1)
          1
          (+ (myFib (- n 1)) (myFib (- n 2)))
      )
  )
)

; Second tutorial

(define (Take number list)
  (if (null? list)
      null
      (if (equal? number 0)
          null
          (cons (car list) (Take (- number 1) (cdr list)))
      )
  )
)

(define (Drop number list)
  (if (null? list)
      null
      (if (equal? number 0)
          (cons (car list) (Drop number (cdr list)))
          (Drop (- number 1) (cdr list))
      )
  )
)

(define (Append list value)
  (if (null? list)
      (cons value null)
      (cons (car list) (Append (cdr list) value))
  )
)

(define (Concat listA listB)
  (if (null? listA)
      listB
      (cons (car listA) (Concat (cdr listA) listB))
  )
)

(define (Contains list elem)
  (if (null? list)
      false
      (if (equal? (car list) elem)
          true
          (Contains (cdr list) elem)
      )
  )
)

(define (ContainsAll list pattern)
  (if (null? pattern)
      true
      (if (equal? (Contains list (car pattern)) true)
          (ContainsAll list (cdr pattern))
          false
      )
  )
)

(define (Last list)
  (if (null? list)
      null
      (if (equal? (cdr list) null)
          (car list)
          (Last (cdr list))
      )
  )
)

; Third tutorial

(define (myHasElementBST tree elem)
  (if (null? tree)
      false
      (if (< elem (car tree))
          (myHasElement (car (cdr tree)) elem)
          (if (equal? elem (car tree))
              true
              (myHasElement (car (cdr (cdr tree))) elem)
          )
      )
  )
)

(define (myRemoveElement list elem)
  (if (null? list)
      null
      (if (not (equal? (car list) elem))
          (cons (car list) (myRemoveElement (cdr list) elem))
          (myRemoveElement (cdr list) elem)
      )
  )
)

(define (myAddElementBST tree elem)
  (if (null? tree)
      (list elem null null)
      (if (< elem (car tree))
          (list (myAddElementBST (car (cdr tree)) elem) (car (cdr (cdr tree))))
          (if (equal? (car tree) elem)
              tree
              (myAddElementBST (car (cdr tree)) (myAddElementBST (car (cdr (cdr tree))) elem))
          )
      )
  )
)

(define (myRangeSum list min max)
  (if (null? list)
      0
      (if (and (>= (car list) min) (<= (car list) max))
          (+ (car list) (myRangeSum (cdr list) min max))
          (myRangeSum (cdr list) min max)
      )
  )
)

(define (reverseList list new_list)
  (if (null? list)
      new_list
      (reverseList (cdr list) (cons (car list) new_list))
  )
)

; Fourth tutorial
(define (isEven? num)
  (if (equal? num 0)
      true
      (if (equal? num 1)
          false
          (isEven? (- num 2))
      )
  )
)

(define (FilterOutOdd list)
  (filter isEven? list)
)

(define (SumIfOdd list)
  (if (null? list)
      0
      (if (not (isEven? (car list)))
          (+ (car list) (SumIfOdd (cdr list)))
          (SumIfOdd (cdr list))
      )
  )
)

(define (AddIfOdd list elem)
  (map (lambda (num) (if (isEven? num) num (+ num elem))) list)
)

(define (add2 list1 list2)
  (+ list1 list2)
)

(define (toList3 list1 list2 list3)
  (list list1 list2 list3)
)

(define (toListMap list1 list2 list3)
  (map (toList3 list1 list2 list3) list1 list2 list3)
)