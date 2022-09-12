#lang racket/base

(define (square n) (* n n))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons
              answer
              (square (car things))))))
  (iter items '()))

; doesnt work bc nesting ends at the cons cell with the first item,
; as anwer gets put into a cons cell that points to the next number, not pair.
; i don't think theres a way to do this iteratively w/o reversing the output before return. otherwise we'd enter n^2 territory, if we're appending
(define test '(1 2 3 4))

(display (square-list test))
(newline)
