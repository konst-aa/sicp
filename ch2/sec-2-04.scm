(import (srfi 1)
        (srfi 64))

(set! test-log-to-file #f)
(test-runner-factory
  (lambda ()
    (let ((runner (test-runner-simple)))
      ;; If I comment the test-runner-aux-value line and remove
      ;; the wrapping "all-tests thing", I just get a tally
      ;; of the number of passes.
      ;; If I keep it uncommented w/o wrapping,
      ;; I get all the info abt each test case + the tally ...
      ;; ... but only for the first set of tests.
      ;; If I keep it uncommented with wrapping (what im doing rn)
      ;; I get everything. IDK WHY! I don't want ot wrap it looks ugly!
      ;; when I figure this out I prob won't come back to this file, tho.
      (test-runner-aux-value! runner (current-error-port))
      runner)))

(test-begin "all-tests section 2-04")
(test-runner-factory
  (lambda ()
    (let ((runner (test-runner-simple)))
      ;; If I comment the test-runner-aux-value line and remove
      ;; the wrapping "all-tests thing", I just get a tally
      ;; of the number of passes.
      ;; If I keep it uncommented w/o wrapping,
      ;; I get all the info abt each test case + the tally ...
      ;; ... but only for the first set of tests.
      ;; If I keep it uncommented with wrapping (what im doing rn)
      ;; I get everything. IDK WHY! I don't want ot wrap it looks ugly!
      ;; when I figure this out I prob won't come back to this file, tho.
      (test-runner-aux-value! runner (current-error-port))
      runner)))


;;; argh why is this covered in the next chapter >:(
(define global-table (list))

(define (alist-ref key alist)
  (cdr (assoc key alist)))

(define (get row col)
  (alist-ref col (alist-ref row global-table)))

(define (add-row row)
  (set! global-table (alist-cons row (list) global-table)))

(define (put row col datum)
  (set! global-table
    (alist-cons row
                (alist-cons col datum (alist-ref row global-table))
                global-table)))


;;; 2-73
(define variable? symbol?)
(define same-variable? eq?)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else
          ((get 'deriv (operator exp)) (operands exp) var))))

(define operator car)
(define operands cdr)

;; works with the form '(multiplicand multiplier)
;; different from the original '(* multiplicand multiplier)
(define multiplicand car)
(define multiplier cadr)
(define (make-product a b)
  `(* ,a ,b))

(define addend car)
(define augend cadr)
(define (make-sum a b)
  `(+ ,a ,b))

;;; a)
;;; deriv always takes the operands, and the wrt var.
;;; one can't get an "operator" of a number or a variable,
;;; same for operands.

;;; b)
(define (deriv-sum operands var)
  (make-sum (deriv (augend operands) var)
            (deriv (addend operands) var)))

(define (deriv-product operands var)
  (make-sum (make-product (multiplier operands)
                          (deriv (multiplicand operands) var))
            (make-product (deriv (multiplier operands) var)
                          (multiplicand operands))))


(add-row 'deriv)
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)

(define x 1)

;; for x = 1, d/dx of 1 + 5x^2 = 10
(test-equal
  (eval (deriv (make-sum 1 (make-product (make-product 'x 'x) 5)) 'x))
  10)

;;; c)

(define (deriv-exp operands var)
  `(exp ,(car operands)))

(put 'deriv 'exp deriv-exp)

(test-equal
  (deriv '(exp x) x)
  '(exp x))

;;; d)
;;; swap rows and cols. operators will be rows, and derivs will be cols.


(test-end "all-tests section 2-04")
