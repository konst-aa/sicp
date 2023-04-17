(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((equal? x (car set)) #t)
    (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
 (if (element-of-set? x set)
  set
  (cons x set)))

(define (intersection-set set1 set2)
 (cond
  ((null? set1) '()))
  ((element-of-set? (car set1) set2)
   (cons (car set1) (intersection-set (cdr set1) set2)))
  ((else (intersection-set (cdr set1) set2))))

;2-59
(define (union-set set1 set2)
 (cond
  ((null? set1) set2)
  ((not (element-of-set? (car set1) set2))
   (union-set (cdr set1) (cons (car set1) set2)))
  (else (union-set (cdr set1) set2))))

(print (union-set '(1 2 3) '(2 3 4)))
; 2-60
;; element-of-set? implementation stays the same. O(n)

(define adjoin-duplicate-set cons)
;; adding to a duplicate set takes O(1) time

(define union-duplicate-set append) ; low hanging fruit
;; unioning two duplicate sets O(1) time

;; intersection-set implementation stays the same. O(n^2)
;; Duplicate sets are good, but take increasing amounts of space
;; as adjoin-set will always add to a set, and union-set will always
;; make a set of size 2n.
;; This also means intersections will be slow as they're the only procedure 
;; that uses element-of-set? which will also take longer and longer as the
;; duplicate set grows.
;; So, duplicates sets are useful when a lot of "unioning?" needs to be done.
