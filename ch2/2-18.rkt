#lang racket/base

(define (reverse items)
  (define (iter acc current)
    (if (null? current)
      acc
      (iter (cons (car current) acc)
              (cdr current))))
    (iter '() items))

(display (reverse '()))
(newline)

(display (reverse (list 1 4 9 16 25)))
(newline)
