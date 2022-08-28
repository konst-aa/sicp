(import (scheme small))

;; linear
(define (accumulate-lin combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-lin combiner null-value term (next a) b))))

;; iterative, less potato
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a current)
    (if (> a b)
        current
        (iter (next a) (combiner (term a) current))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(display (sum square 0 (lambda (n) (+ n 1)) 5))
(newline)
(display (sum square 6 (lambda (n) (+ n 1)) 5))
