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

(define (recLen1 x)
  (if (null? x)
      0
      (if (atom? (car x))
          (+ 1 (recLen (cdr x)))
          (if (equal? (recLen1 (car x)) 0)
              (+ 1 (recLen1 (cdr x)))
              (+ (recLen1 (car x)) (recLen1 (cdr x)))
          )
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

(define (myHasElement x n c)
  (if (equal? c true)
      true
      (if (null? x)
          false
          (if (atom? (car x))
              (if (equal? (car x) n)
                  (myHasElement (cdr x) n true)
                  (myHasElement (cdr x) n false)
              )
              (or (myHasElement (car x) n false)
               (myHasElement (cdr x) n false)
              )
          )
      )
  )
)

(define (myNthElement x n counter)
  (if (null? x)
      "number of elements is less than the index indicated"
      (if (equal? counter n)
          (if (atom? (car x))
              (car x)
              (myNthElement (car x) n counter)
          )
          (if (atom? (car x))
              (myNthElement (cdr x) n (+ 1 counter))
              (myNthElement (car x) n counter)
          )
      )
  )
)

(define (myMin a b)
  (if (< a b)
      a
      b
  )
)

(define (myMax a b)
  (if (> a b)
      a
      b
  )
)

(define (myMinL x min counter)
  (if (null? x)
      min
      (if (equal? 0 counter)
          (myMaxL (cdr x) (car x) (+ 1 counter))
          (if (< (car x) min)
              (myMinL (cdr x) (car x) (+ 1 counter))
              (myMinL (cdr x) min (+ 1 counter))
          )
      )
  )
)

(define (myMaxL x max counter)
  (if (null? x)
      max
      (if (equal? 0 counter)
          (myMaxL (cdr x) (car x) (+ 1 counter))
          (if (> (car x) max)
              (myMaxL (cdr x) (car x) (+ 1 counter))
              (myMaxL (cdr x) max (+ 1 counter))
          )
      )
  )
)

(define (myFact n)
  (if (or (equal? 1 n) (equal? 0 n))
      1
      (* n (myFact (- n 1)))
  )
)

(define (myFib n)
  (if (or (equal? 0 n))
      0
      (if (equal? 1 n)
          1
          (+ (myFib (- n 1)) (myFib(- n 2)))
      )
  )
)

; Second tutorial
(define (Take counter list)
  (if (null? list)
      `()
      (if (equal? 0 counter)
          `()
          (cons (car list) (Take (- counter 1) (cdr list)))
      )
  )
)

(define (Drop n list)
  (if (null? list)
      `()
      (if (equal? 0 n)
          list
          (Drop (- n 1) (cdr list))
      )
  )
)

(define (Append list n)
  (if (equal? list `())
      (cons n `())
      (cons (car list) (Append (cdr list) n))
  )
)

(define (Concat listA listB)
  (if (null? listA)
      listB
      (cons (car listA) (Concat (cdr listA) listB))
  )
)

(define (ContainsAllHelper val patterns)
  (if (null? patterns)
      false
      (if (equal? val (car patterns))
          true
          (ContainsAllHelper val (cdr patterns))
      )
  )
)

(define (ContainsAll listA patterns)
  (if (null? listA)
      (if (null? patterns)
          true
          false
      )
      (if (null? patterns)
          true
          (if (equal? (ContainsAllHelper (car listA) patterns) true)
              (ContainsAll (cdr listA) (cdr patterns))
              (ContainsAll (cdr listA) patterns)
          )
      )
  )
)

(define (Last list)
  (if (equal? list `())
      null
      (if (null? (cdr list))
          (car list)
          (Last (cdr list))
      )
  )
)

(define (hasElement list val)
  (if (null? list)
      false
      (if (equal? (car list) val)
          true
          (hasElement (cdr list) val)
      )
  )
)

(define (myHasElementBST tree val)
  (if (null? tree)
      false
      (if (equal? (car tree) val)
          true
          (if (< val (car tree))
              (myHasElementBST (car (cdr tree)) val)
              (myHasElementBST (car (cdr (cdr tree))) val)
          )
      )
  )
)

(define (myRemoveElement list val)
  (if (null? list)
      null
      (if (equal? (car list) val)
          (myRemoveElement (cdr list) val)
          (cons (car list) (myRemoveElement (cdr list) val))
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

(define (reverseList list reversedList)
  (if (null? list)
      reversedList
      (reverseList (cdr list) (cons (car list) reversedList))
  )
)