(import (srfi 64))

(set! test-log-to-file #f)
(test-runner-factory
  (lambda ()
    (let ((runner (test-runner-simple)))
      (test-runner-aux-value! runner (current-error-port))
      runner)))



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

;; 2-61
(define (adjoin-ordered-set x s1)
  (cond
    ((null? s1) (list x))
    ((= x (car s1)) s1)
    ((< x (car s1)) (cons x s1))
    (else
      (cons (car s1) (adjoin-ordered-set x (cdr s1)))))
  )


(print "2-61")
(print (adjoin-ordered-set 1 (list)) (list 1))
(print (adjoin-ordered-set 1 (list)) (list 2))
(print (adjoin-ordered-set 4 (list 1 2 3 4)) (list 1 2 3 4))
(print (adjoin-ordered-set 0 (list 1 2 3 4)) (list 0 1 2 3 4))
(print (adjoin-ordered-set 3 (list 1 2 3 4)) (list 1 2 3 4))

(newline)

;; 2-62
(define (ordered-union s1 s2)
  (cond
    ((null? s1) s2)
    ((null? s2) s1)
    ((< (car s1) (car s2))
     (cons (car s1) (ordered-union (cdr s1) s2)))
    ((> (car s1) (car s2))
     (cons (car s2) (ordered-union s1 (cdr s2))))
    (else
      (cons (car s1) (ordered-union (cdr s1) (cdr s2))))))

(print "2-62")
(print (ordered-union (list 1 2 3 4) (list 1 2 3))
       (list 1 2 3 4))
(print (ordered-union (list 0 1 2 3 4 5) (list 1 6))
       (list 0 1 2 3 4 5 6))

(newline)


;; 2-63
(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (make-node item l r)
  (list item l r))

(define (make-leaf item)
  (make-node item (list) (list)))

(define (tree->list-1 tree)
  (if (null? tree)
    (list)
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree (list)))


;;; they do the same thing, except tree->list-1 is slower
;;; as `append` takes O(n) and stuff gets joined multiple times,
;;; time, while tree->list-2 feeds directly into the cons cells
;;; feels like O(nlog(n)) vs O(n)
;;; they do the same thing, because:
;;; - they have the same base case
;;; - they have the same divide and conquer approach;
;;;   first, they tree->list the left and right branches
;;;   then they sandwich the entry of the node between them.
;;;   `copy-to-list` effectively reads 'add the tree' to the start
;;;   of a list, it's an accumulator approach to the problem
;;;   which greatly speeds up the solution

(define tree1
  (make-node
    7
    (make-node 3
               (make-leaf 1)
               (make-leaf 5))
    (make-node 9
               (list)
               (make-leaf 11))))

(define tree2
  (make-node
    3
    (make-leaf 1)
    (make-node
      7
      (make-leaf 5)
      (make-node
        9
        (list)
        (make-leaf 11)))))

(define tree3
  (make-node
    5
    (make-node
      3
      (make-leaf 1)
      (list))
    (make-node
      9
      (make-leaf 7)
      (make-leaf 11))))

(test-begin "2-63")
(test-equal "1st tree converted the same"
            (tree->list-1 tree1)
            (tree->list-2 tree1))
(test-equal "2nd tree"
            (tree->list-1 tree2)
            (tree->list-2 tree2))
(test-equal "3rd tree"
            (tree->list-1 tree3)
            (tree->list-2 tree3))
(test-begin "2-63")



;;; 2-64

(define sorted-tree1 (tree->list-1 tree1))

(define (list->tree elems)
  (car (partial-tree elems (length elems))))

(define (partial-tree elems n)
  (if (= n 0)
    `(() . ,elems)
    (let* ((left-size (quotient (- n 1) 2))
           (left-result (partial-tree elems left-size))
           (left-tree (car left-result))
           (non-left-elems (cdr left-result))
           (right-size (- n (+ left-size 1)))
           (this-entry (car non-left-elems))
           (right-result (partial-tree (cdr non-left-elems) right-size))
           (right-tree (car right-result))
           (remaining-elems (cdr right-result)))
      (cons (make-node this-entry left-tree right-tree)
            remaining-elems))))



(test-begin "2-64")
(test-equal
  "tree produced"
  (list->tree sorted-tree1)
  (make-node
    5
    (make-node
      1
      (list)
      (make-leaf 3))
    (make-node 9
      (make-leaf 7)
      (make-leaf 11))))

(test-end "2-64")

;;; a)
;;; partial-tree creates a tree with a divide & conquer approach
;;; the left tree is made from the first half of the elements
;;; the right subtree is made from the second half of the elements
;;; and the node value is the first part of the remaining right half list.
;;; the 'remaining-elems' is just everything after the first n items
;;;        5
;;;     /   \
;;;    1      9
;;;    \    /  \
;;;     3  7   11
;;; b)
;;; O(n) time
;;; Every split on its own traverses in constant time,
;;; since it just takes cars and cdrs of the recursive calls.
;;; the number of base cases is linear, and 
;;; input list.


;;; 2-65
