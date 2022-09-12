#lang racket/base
; bruh I used racket for this bc chibi wasn't compiling too tired...

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define zero-quote `(lambda (f) (lambda (x) x)))
(define one-quote `(lambda (f) (lambda (x) (f x))))
(define (add-one church-numeral)
    (lambda (f)
            (lambda (x)
             (f ((church-numeral f) x)))))

(define (add-one-quote church-numeral)
    `(lambda (f)
            (lambda (x)
             (f ((,church-numeral f) x)))))

(define (add-numerals cn1 cn2)
    (lambda (f)
        (lambda (x)
            ((cn1 f) 
             ((cn2 f) x)))))

(define (add-numerals-quote cn1 cn2)
    `(lambda (f)
        (lambda (x)
            ((,cn1 f) 
             ((,cn2 f) x)))))

(display (add-one-quote zero-quote))
(newline)
(display (add-numerals-quote one-quote one-quote))
(newline)