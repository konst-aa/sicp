(import (scheme small))

;; linear
(define (accumulate-lin combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-lin combiner null-value term (next a) b))))

;; iterative, extreme potato
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a current)
    (if (> a b)
        (combiner null-value current)
        (iter (next a) (combiner (term a) current))))
  (if (> a b)
      null-value
      (iter (next a) (term a))))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(display (sum square 0 (lambda (n) (+ n 1)) 5))
(newline)
(display (sum square 6 (lambda (n) (+ n 1)) 5))
