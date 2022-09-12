(import (scheme small))

(define (square n) (* n n))

(define (square-tree tree)
  (map (lambda (node)
         (if (list? node)
           (square-tree node)
           (square node)))
       tree))

(display (square-tree
           (list 1
                 (list 2 (list 3 4) 5)
                 (list 6 7))))
(newline)
