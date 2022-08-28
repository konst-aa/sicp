(import (scheme small))

; no higher order fns or lists allowed yet :((
(define (pascal-elem position layer-index)
  (if (or (= position 0) (>= position layer-index))
      1
      (+ (pascal-elem (- position 1) (- layer-index 1))
         (pascal-elem (+ position 1) (- layer-index 1)))))

(display (pascal-elem 1 3))
