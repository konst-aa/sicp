(import (scheme small))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cooler-integration f a b n)
  (define h (/ (- b a) n))
  (define (integration-term k)
    (* (if (even? k) 2 4)
       (f (+ a (* k h)))))
  (* (/ h 3)
     (+ (f a)
        (sum integration-term
             1
             (lambda (k) (+ k 1))
             n))))

;; seems legit, chibi does fractions tho
(display (cooler-integration (lambda (x) (* x x x)) 0 1 100))
(display (cooler-integration (lambda (x) (* x x x)) 0 1 1000))
