(import (scheme small))

(define x (list (list 1 2) (list 3 4)))

(define (deep-reverse items)
  (reverse
    (map (lambda (item)
           (if (list? item)
             (deep-reverse item)
             item))
         items)))

(display (deep-reverse x))
(newline)
