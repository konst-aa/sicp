(define (double proc)
  (lambda (arg) (proc (proc arg))))

(define (inc n)
  (+ n 1))

(display ((double inc) 3))

(newline)

(display (((double (double double)) inc) 5))