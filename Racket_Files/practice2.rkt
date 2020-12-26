#lang racket

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))
  )
)

(define (lengthList lst)
  (if (null? lst)
      0
      (+ 1 (lengthList (cdr lst)))
  )
)

(define (lengthList2 lst)
  (if (null? lst)
      0
      (if (atom? (car lst))
          (+ 1 (lengthList2 (cdr lst)))
          (+ (lengthList2 (car lst)) (lengthList2 (cdr lst)))
      )
  )
)

(define (mySum lst)
  (if (null? lst)
      0
      (if (atom? (car lst))
          (+ (car lst) (mySum (cdr lst)))
          (+ (mySum (car lst)) (mySum (cdr lst)))
      )
  )
)

(define (myHasElement lst e)
  (if (null? lst)
      false
      (if (equal? (car lst) e)
          (car lst)
          (myHasElement (cdr lst) e)
      )
  )
)

(define (myNthElement lst n)
  (if (null? lst)
      "out of bounds of the list"
      (if (equal? n 0)
          (car lst)
          (myNthElement (cdr lst) (- n 1))
      )
  )
)

(define (myMinListHelper lst min counter)
  (if (null? lst)
      min
      (if (or (equal? counter 0) (< (car lst) min))
          (myMinListHelper (cdr lst) (car lst) (+ counter 1))
          (myMinListHelper (cdr lst) min (+ counter 1))
      )
  )
)

(define (myMinList lst)
  (if (null? lst)
      false
      (myMinListHelper lst 0 0)
  )
)

(define (myMaxListHelper lst max counter)
  (if (null? lst)
      max
      (if (or (equal? counter 0) (> (car lst) max))
          (myMaxListHelper (cdr lst) (car lst) (+ counter 1))
          (myMaxListHelper (cdr lst) max (+ counter 1))
      )
  )
)

(define (myMaxList lst)
  (if (null? lst)
      false
      (myMaxListHelper lst 0 0)
  )
)

(define (myFactorial n)
  (if (equal? n 0)
      1
      (* n (myFactorial (- n 1)))
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

(define (Take n lst)
  (if (null? lst)
      null
      (if (equal? n 0)
          null
          (cons (car lst) (Take (- n 1) (cdr lst)))
      )
  )
)

(define (Drop n lst)
  (if (null? lst)
      null
      (if (equal? n 0)
          (cons (car lst) (Drop n (cdr lst)))
          (Drop (- n 1) (cdr lst))
      )
  )
)

(define (Append lst e)
  (if (null? lst)
      (cons e null)
      (cons (car lst) (Append (cdr lst) e))
  )
)

(define (Concat listA listB)
  (if (null? listA)
      listB
      (cons (car listA) (Concat (cdr listA) listB))
  )
)

(define (ContainsElement lst e)
  (if (null? lst)
      false
      (if (equal? (car lst) e)
          true
          (ContainsElement (cdr lst) e)
      )
  )
)

(define (ContainsAll listA listB)
  (if (null? listB)
      true
      (if (not (equal? (ContainsElement listA (car listB)) true))
          false
          (ContainsAll listA (cdr listB))
      )
  )
)

(define (Last lst)
  (if (null? lst)
      null
      (if (equal? (cdr lst) null)
          (car lst)
          (Last (cdr lst))
      )
  )
)

(define (myHasElementBST tree e)
  (if (null? tree)
      false
      (if (equal? (car tree) e)
          true
          (if (< e (car tree))
              (myHasElementBST (car (cdr tree)) e)
              (myHasElementBST (car (cdr (cdr tree))) e)
          )
      )
  )
)

(define (myRemoveElement lst e)
  (if (null? lst)
      null
      (if (equal? (car lst) e)
          (myRemoveElement (cdr lst) e)
          (cons (car lst) (myRemoveElement (cdr lst) e))
      )
  )
)

(define (myAddElementBST tree e)
  (if (null? tree)
      (list e null null)
      (if (equal? (car tree) e)
          (list tree)
          (if (< e (car tree))
              (list (car tree) (myAddElementBST (car (cdr tree)) e) (car (cdr (cdr tree))))
              (list (car tree) (car (cdr tree)) (myAddElementBST (car (cdr (cdr tree))) e))
          )
      )
  )
)

(define (myRangeSum lst min max)
  (if (null? lst)
      0
      (if (and (>= (car lst) min) (<= (car lst) max))
          (+ (car lst) (myRangeSum (cdr lst) min max))
          (myRangeSum (cdr lst) min max)
      )
  )
)

(define (myMulti listA listB)
  (if (null? listA)
      listB
      (if (null? listB)
          listA
          (cons (* (car listA) (car listB)) (myMulti (cdr listA) (cdr listB)))
      )
  )
)

(define (myMerge listA listB)
  (if (null? listA)
      listB
      (if (null? listB)
          listA
          (if (< (car listA) (car listB))
              (cons (car listA) (cons (car listB) (myMerge (cdr listA) (cdr listB))))
              (cons (car listB) (cons (car listA) (myMerge (cdr listA) (cdr listB))))
          )
      )
  )
)

(define (removeFirstOccurence lst e)
  (if (null? lst)
      null
      (if (equal? (car lst) e)
          (cdr lst)
          (cons (car lst) (removeFirstOccurence (cdr lst) e))
      )
  )
)

(define (findMin lst min counter)
  (if (null? lst)
      min
      (if (or (equal? counter 0) (< (car lst) min))
          (findMin (cdr lst) (car lst) (+ counter 1))
          (findMin (cdr lst) min (+ counter 1))
      )
  )
)

(define (posOfElem lst e counter)
  (if (null? lst)
      0
      (if (equal? (car lst) e)
          counter
          (posOfElem (cdr lst) e (+ counter 1))
      )
  )
)

(define (Swap lst elem pos)
  (if (null? lst)
      null
      (if (equal? pos 0)
          (cons elem (Swap (cdr lst) elem (- pos 1)))
          (cons (car lst) (Swap (cdr lst) elem (- pos 1)))
      )
  )
)

(define (mySelectSort lst)
  (if (null? lst)
      null
      (let ((min (findMin lst 0 0)))
        (let ((pos (posOfElem (cdr lst) min 0)))
          (if (equal? (car lst) min)
              (cons (car lst) (mySelectSort (cdr lst)))
              (cons min (mySelectSort (Swap (cdr lst) (car lst) pos)))
          )
        )
      )
  )
)

(define (reverseListHelper lst new_lst)
  (if (null? lst)
      new_lst
      (reverseListHelper (cdr lst) (cons (car lst) new_lst))
  )
)

(define (reverseList lst)
  (reverseListHelper lst null)
)

(define (atom1? x)
  (and (not (null? x))
       (not (pair? x))
  )
)

(define (appendList listA listB)
  (if (null? listA)
      listB
      (cons (car listA) (appendList (cdr listA) listB))
  )
)

(define (flattenList lst)
  (if (null? lst)
      null
      (if (atom1? (car lst))
          (cons (car lst) (flattenList (cdr lst)))
          (appendList (flattenList (car lst)) (flattenList (cdr lst)))
      )
  )
)

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))
  )
)

(define (flattenList1 lst)
  (if (null? lst)
      null
      (if (atom? (car lst))
          (cons (car lst) (flattenList1 (cdr lst)))
          (appendList (flattenList1 (car lst)) (flattenList1 (cdr lst)))
      )
  )
)