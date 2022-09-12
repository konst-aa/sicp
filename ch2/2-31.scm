(import (scheme small))

(define (tree-map proc tree)
  (map (lambda (node)
         (if (list? node)
           (tree-map proc node)
           (proc node)))
       tree))

(display (tree-map (lambda (n) (* n n))
                   (list 1
                         (list 2 (list 3 4) 5)
                         (list 6 7))))
(newline)
