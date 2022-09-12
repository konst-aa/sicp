(define (make-rat n d)
        (if (or (negative? n)
        (negative? d))
        (cons (* -1 (abs n))
            (abs d))
        (cons n d)))

(define (display-rat rat)
  (display (car rat))
  (display "/")
  (display (cdr rat)))

(display-rat (make-rat 3 -4))

(newline)