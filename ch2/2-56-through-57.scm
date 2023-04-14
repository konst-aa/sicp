(define variable? symbol?)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? e)
  (and (pair? e) (eq? (car e) '+)))

(define (addend exp)
  (cadr exp))

(define (augend exp)
  (if (equal? (cdddr exp) '())
    (caddr exp)
    (cons '+ (cddr exp))))

(define (make-sum a1 a2)
  (cond
    ((eqv? a1 0) a2)
    ((eqv? a2 0) a1)
    ((and (number? a1) (number? a2)) (+ a1 a2))
    (else (list '+ a1 a2)))) ; orr `(+ ,a1 , a2)

(define (product? e)
  (and (pair? e) (eq? (car e) '*)))

(define (multiplier exp)
 (cadr exp))

(define (multiplicand exp)
  (if (equal? (cdddr exp) '())
    (caddr exp)
    (cons '* (cddr exp))))

(define (make-product m1 m2)
  (cond
    ((or (eqv? m1 0) (eqv? m2 0)) 0)
    ((eqv? m1 1) m2)
    ((eqv? m2 1) m1)
    ((and (number? m1) (number? m2)) (* m1 m2))
    (else (list '* m1 m2)))) ; orr `(* ,m1 , m2)

;; 2-56
(define (** b e)
  (if (= e 0)
    1
    (* b (** b (- e 1)))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define base cadr)
(define exponent caddr)

(define (make-exponent b e)
  (cond
    ((eqv? e 0) 1)
    ((eqv? e 1) b)
    ((and (number? b) (number? e)) (** b e))
    (else (list '** b e))))

(define (deriv exp var)
  (cond
    ((number? exp) 0)
    ((variable? exp)
     (if (same-variable? exp var) 1 0))
    ((sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)))
    ((product? exp)
     (make-sum
       (make-product (multiplicand exp)
                     (deriv (multiplier exp) var))
       (make-product (deriv (multiplicand exp) var)
                     (multiplier exp))))
    ((exponentiation? exp)
     (make-product
       (exponent exp)
       (make-product
         (make-exponent (base exp)
                        (make-sum (exponent exp) -1))
         (deriv (base exp) 'x))))
    (else (error "unkown expression type -- DERIV" exp))))


(display (deriv '(+ x 3) 'x))
(newline)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
;; 2-56
(display (deriv '(** x y) 'x))
(newline)
(display (deriv '(** x 2) 'x))
(newline)
;; 2-57
(display (deriv '(* x y (+ x 3)) 'x))
(newline)
(display (deriv '(* x (* x x)) 'x))
