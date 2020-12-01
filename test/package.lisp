(in-package #:cl-user)

(defpackage #:rfc3339-timestamp-test
 (:use #:cl #:rfc3339)
 (:import-from #:lisp-unit #:define-test #:assert-equal)
 (:export #:run-tests))
