(defsystem rfc3339-timestamp
  :name "A RFC3339 timestamp library"
  :version "0.1.3"
  :license "BSD"
  :author "Peter Stiernström <peter@stiernstrom.se>"
  :description "A Common Lisp implementation of timestamps as defined by RFC3339"
  :depends-on ( :yacc )
  :serial t
  :components ((:file "src/package")
               (:file "src/timestamp")
               (:file "src/parser")))
