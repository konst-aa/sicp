(import (scheme small))

;; underwhelming? i thought I had to make this run in log time lol
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(display (sum square 0 (lambda (n) (+ n 1)) 5))
