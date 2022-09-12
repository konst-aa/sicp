#lang racket/base

(define (for-each proc items)
  (if (null? items)
    #t
    ; definitely not begin
    ((lambda ()
       (proc (car items))
       (for-each proc (cdr items))))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
