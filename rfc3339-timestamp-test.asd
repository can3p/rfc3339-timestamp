(defsystem rfc3339-timestamp-test
 :name "rfc3339-timestamp test suite"
 :version "0.2.5"
 :license "BSD"
 :author "Peter Stiernström <peter@stiernstrom.se>"
 :description "Implementation of a test suite with the additional dependency on lisp-unit"
 :depends-on ( :rfc3339-timestamp :lisp-unit )
 :serial t
 :components ((:module :test
               :serial t
               :components ((:file "package")
                            (:file "suite")))))

