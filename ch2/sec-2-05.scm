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
      ;; IDK what I was writing above ngl
      (test-runner-aux-value! runner (current-error-port))
      runner)))

(test-begin "all-tests section 2-05")
(display "I wrote no code for this. I only looked at the questions and read stuff"
         (current-error-port))
(newline (current-error-port))

;;; More generics
;;; 2.81
;;; a) if there were to be a coercion identity operation, then
;;; calling a non-existent procedure for two arguments
;;; with an identical type would result in an infinite loop!
;;; the procedure lookup fails, and arg1 gets coerced to the type of arg2
;;; which is the same type. Then apply-generic gets called with the
;;; same proc and the same argument types. We're back to where we started
;;; b) everything works as is, because if the program attempts
;;; self-coercion, it should get a falsy value, which would prevent
;;; the cond statement from trying the same proc with the same arguments.
;;; then it would also fail for coercing arg2 to the type of arg1,
;;; and eventually it would raise an error.
;;; 2.84
;;; b) ... raising the lowest (in the tower) argument could be done this way:
;;; start at the bottom of the tower.
;;; does the type we're on match arg1? if yes, raise arg1 until it matches the type of arg2
;;; does it match arg2? same as above, except raise arg2 instead.
;;; otherwise, go up a level

;;; ack polynomials and stuff like that

(test-end "all-tests section 2-05")
