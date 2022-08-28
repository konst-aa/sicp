(import (scheme small))

; first trying to impl logarithmic time exponents
(define (quick-exp n counter)
  (cond
   ((= counter 0) 1)
   ((even? counter) (square (quick-exp n (/ counter 2))))
   (else (* n (quick-exp n (- counter 1))))))

(display (quick-exp 3 10))

;; this took some thinking
(define (fib-iter a b p q count)
  (cond
   ((= count 0) b)
   ((even? count)
    (fib-iter a
              b
              (+ (square p) (square q))
              (+ (* 2 p q) (square q))
              (/ count 2)))
   (else (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))

(newline)
(display (fib-iter 1 0 0 1 10))
