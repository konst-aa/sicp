#lang racket/base

(define (square n)
  (* n n))

(define (square-list-overflow items)
  (if (null? items)
    '()
    (cons (square (car items))
          (square-list-overflow (cdr items)))))

(define (square-list-map items)
  ; we do a little trolling :-)
  (map * items items))

(define test '(1 2 3 4))

(display (square-list-overflow test))
(newline)

(display (square-list-map test))
(newline)
