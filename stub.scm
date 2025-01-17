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

(display "Section X.Y: something")
(newline)
(newline)

(test-begin "Problem X.ZZ")
; (display "I wrote no code for this. I only looked at questions and read stuff"
;          (current-error-port))
; (newline (current-error-port))

(test-equal 'a 'a)

(test-end "Problem X.ZZ")
