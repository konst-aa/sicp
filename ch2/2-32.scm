(import (scheme small)
        (srfi 1))

; this wasnt too bad after i caught up on sleep
; didnt catch up that well tbf

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((next (subsets (cdr s))))
      (append
        next
        (map (lambda (subset)
               (cons (car s) subset))
             next)))))

(display (subsets '(1 2 3)))
(newline)