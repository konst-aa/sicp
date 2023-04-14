(define (equal? a b)
  (cond
    ((and (atom? a) (atom? b))
     ; ^ idk if this is part of IEEE scheme bc the standard is paywalled ^
     (eq? a b))
    ((and (list? a) (list? b))
     (and (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))))

(display (equal? '(this is a list) '(this is a list)))
(display (equal? '(this (is a) list) '(this is a list)))
