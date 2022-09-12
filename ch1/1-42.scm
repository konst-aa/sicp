(define (comp f g)
  (lambda (args ...)
    (f (apply g (list args ...)))))

(define (add-sq a b)
  ((comp (lambda (n) (* n n)) +) a b))

(display (add-sq 4 6))