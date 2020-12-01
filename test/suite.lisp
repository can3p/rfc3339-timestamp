(in-package #:rfc3339-timestamp-test)

(defun run-tests ()
 "Run lisp-unit tests"
 (let ((results (lisp-unit:run-tests :all :rfc3339-timestamp-test)))
  (lisp-unit:print-failures results)
  (lisp-unit:print-errors results)))

(define-test sanity-check
 (assert-equal 0
  (- (encode-universal-time 0 0 0 1 1 2000 0)
   (rfc3339:utc-time-of
    (rfc3339:parse-string "2000-01-01T00:00:00Z"))))
 (assert-equal 3600
  (- 
   (rfc3339:local-time-of (rfc3339:parse-string "2000-01-01T00:00:00+06:00"))
   (rfc3339:local-time-of (rfc3339:parse-string "2000-01-01T00:00:00+05:00"))))
 (assert-equal 7200
  (- 
   (rfc3339:local-time-of (rfc3339:parse-string "2000-01-01T00:00:00+01:00"))
   (rfc3339:local-time-of (rfc3339:parse-string "2000-01-01T00:00:00-01:00"))))
 (assert-equal 7200
  (- 
   (rfc3339:local-time-of (rfc3339:parse-string "2000-01-01T00:00:00.0+01:00"))
   (rfc3339:local-time-of (rfc3339:parse-string "2000-01-01T00:00:00.0-01:00"))))
 (assert-equal 500
  (- 
   (rfc3339:fraction-of (rfc3339:parse-string "2000-01-01T00:00:00.500Z"))
   (rfc3339:fraction-of (rfc3339:parse-string "2000-01-01T00:00:00Z")))))

