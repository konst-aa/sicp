#lang racket

(define (flatmap proc items)
  (foldr append '() (map proc items)))

(define (enumerate-interval start end)
  (if (> start end)
      '()
      (cons start (enumerate-interval (+ start 1) end))))

(define empty-board '())

(define (zip args ...)
  (define shortest
    (apply min (map length (list args ...))))
  (apply map list (map (lambda (items)
                               (take items shortest))
                       (list args ...))))

(define (queens board-size)
  (define (adjoin-position new-row k rest)
    (cons `(,new-row ,k) rest))
  (define (safe? k positions)
    (define (in-row? row positions)
      (foldr (lambda (pos acc)
                     (or acc (= (car pos) row)))
             #f
             positions))
    (define (in-dia? row col row-inc col-inc positions)
      (define (seq-from-inc curr inc)
        (if (or (< curr 1)
                (> curr board-size))
            '()
            (cons curr (seq-from-inc (+ curr inc) inc))))
      (let ((possibles (zip (seq-from-inc row row-inc)
                            (seq-from-inc col col-inc))))
           (foldr (lambda (possible acc)
                          (or acc (member possible positions)))
                  #f
                  possibles)))
    (let* ((our-queen (car positions))
           (new-positions (cdr positions)))
          (let ((row (car our-queen))
                (col (cadr our-queen)))
               (not (or (in-row? row new-positions)
                        (in-dia? row col 1 1 new-positions)
                        (in-dia? row col 1 -1 new-positions)
                        (in-dia? row col -1 1 new-positions)
                        (in-dia? row col -1 -1 new-positions))))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
                 (safe? k positions))
         (flatmap
          (lambda (rest)
                  (map (lambda (new-row)
                               (adjoin-position new-row k rest))
                       (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

  ; ENUMERATE INTERVAL IS INCLUSIVE????
  
  ((lambda (_) "done" ) (map displayln (queens 8)))
