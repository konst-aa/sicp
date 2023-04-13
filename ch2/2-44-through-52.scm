; from the picture language example

; textbook
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

; 2-44
; assuming below is (below bottom top)
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below (beside smaller smaller) painter))))

; 2-45
(define (split place-identity place-smaller)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((split-proc (split place-identity place-smaller))
            (smaller (split-proc painter (- n 1))))
        (place-identity (place-smaller smaller smaller) painter)))))

; textbook
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (x-cor-vect v)
                            (edge1-frame frame))
                (scale-vect (y-cor-vect v)
                            (edge2-frame frame))))))

; 2-46
(define make-vect cons)
(define x-cor-vect car)
(define y-cor-vect cdr)

(define (vector-op op)
  (lambda (v1 v2)
    (let ((new-x (op (x-cor-vect v1) (x-cor-vect v2)))
          (new-y (op (y-cor-vect v1) (y-cor-vect v2))))
      (make-vect new-x new-y))))

(define add-vect (vector-op +))
(define sub-vect (vector-op -))
(define (scale-vect v s)
  (define cheeky (vector-op *))
  (cheeky v (make-vect s s)))

; (display (scale-vect (make-vect 1 2) 4))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define frame-origin car)
(define frame-edge1 cadr)
(define frame-edge2 caddr)

; (define (make-frame origin edge1 edge2)
;   (cons origin (cons edge1 edge2)))

; (define frame-origin car)
; (define frame-edge1 cadr)
; (define frame-edge2 cddr)

; (define test-frame
;   (make-frame (make-vect 0 1)
;               (make-vect 2 3)
;               (make-vect 4 5)))

; (display (frame-origin test-frame))
; (display (frame-edge1 test-frame))
; (display (frame-edge2 test-frame))

; textbook
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

; 2-48

(define make-segment cons)
(define start-segment car) ; i like segment-start more but oh well
(define end-segment cdr)

; 2-49
(define outline-painter
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 0 1))
          (make-segment (make-vect 0 0) (make-vect 1 0))
          (make-segment (make-vect 1 1) (make-vect 0 1))
          (make-segment (make-vect 1 1) (make-vect 1 0)))))

(define x-painter
  (segments->painter
    (list (make-segment (make-vect 0 0) (make-vect 1 1))
          (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond-painter
  (segments->painter
    (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
          (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
          (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
          (make-segment (make-vect 1 0.5) (make-vect 0.5 0)))))
; I signed up to sling abstactions, not for art class
; RIP wave, never drawn

; textbook
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter
        (make-frame new-origin
                    (sub-vect (m corner1) new-origin)
                    (sub-vect (m corner2) new-origin))))))

; 2-50 bruh it's counterclockwise
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (rotate-180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate-270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

; 2-51
(define (below painter1 painter2)
  (let* ((split-point (make-vect 0 0.5))
         (paint-bottom
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 1 0)
                              split-point))
         (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-right frame))))

; checked against https://github.com/danielpi/SICP-Exercises/blob/master/Racket/2.2.4%20A%20Picture%20Language%20Example.rkt
; a key difference is that the link has (below top bottom) while the question asks for (below bottom top)

(define (below1 painter1 painter2)
 (rotate-90 (beside (rotate-270 painter1) (rotate-270 painter1))))


