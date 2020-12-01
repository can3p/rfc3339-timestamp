(in-package #:rfc3339)

;; Some small utility functions used in the yacc parser.

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun flatten (tree)
    (let (list)
      (labels ((traverse (subtree)
                 (when subtree
                   (if (consp subtree)
                       (progn
                         (traverse (car subtree))
                         (traverse (cdr subtree)))
                       (push subtree list)))))
        (traverse tree))
      (nreverse list)))
  
  (defun the-values (&rest values)
    values)

  (defun unseparate (&rest values)
    (loop :for v :in values :for idx :from 1 :when (oddp idx) :collect v))

  (defun unseparate-butlast (&rest values)
    (append (apply #'unseparate values) (last values)))

  (defun collapse (&rest ys)
    (parse-integer (coerce ys 'string)))

  (defun collapse-butfirst (&rest parts)
    (apply #'collapse (reverse (butlast (reverse parts)))))

  (defun offset-seconds (&rest values)
    (let ((sign (first values))
          (hour (second values))
          (minute (fourth values)))
      (funcall sign 0 (+ (* hour 3600) (* minute 60)))))

  (defun create-timestamp (&rest parts)
   (destructuring-bind ((year month day) big-t ((hour minute second &optional maybe-fraction) (&optional maybe-offset))) parts
    (make-instance
     'timestamp
     :utc-time (encode-universal-time second minute hour day month year 0)
     :fraction (or maybe-fraction 0)
     :offset (or maybe-offset 0))))
 )

;; The parser accepts dates with or without fraction of a second as
;; well as with or without timezone in which case UTC is assumed. The
;; date/time seperator is either the expected T or a single space
;; characther. Some legal examples follow:
;;
;; 1900-01-01T00:00:00Z
;; 1900-01-01T00:00:00.0Z
;; 1900-01-01T00:00:00.0+00:00
;; 
;; 1900-01-01 00:00:00Z
;; 1900-01-01 00:00:00.0Z
;; 1900-01-01 00:00:00.0+00:00
;;
;; Also parses the 'iso8601 inspired' version:
;; 
;; 19000101T00:00:00
;;
;; If you want a stricter parser just push :rfc3339-strict-parser to
;; *features* and recompile

(define-parser *rfc3339-parser*
  (:start-symbol date-time)
  (:terminals (digit big-t minus colon plus dot zulu))

  (date-time
   (full-date big-t full-time #'create-timestamp)
   #-:rfc3339-strict-parser
   (xmlrpc-full-date big-t xmlrpc-partial-time #'create-timestamp))
  
  (full-date
   (date-fullyear minus date-month minus date-mday #'unseparate))
  
  #-:rfc3339-strict-parser
  (xmlrpc-full-date
   (date-fullyear date-month date-mday #'the-values))
  
  #-:rfc3339-strict-parser
  (xmlrpc-partial-time
   (time-hour colon time-minute colon time-second #'unseparate))
  
  (full-time
   (partial-time time-offset #'the-values))

  (partial-time
   (time-hour colon time-minute colon time-second time-secfrac #'unseparate-butlast)
   (time-hour colon time-minute colon time-second #'unseparate))
  
  (time-offset
   (zulu #'the-values)
   (time-numoffset #'the-values))

  (time-numoffset
   (plus time-hour colon time-minute #'offset-seconds)
   (minus time-hour colon time-minute #'offset-seconds))

  ;; Allow for up to ten additional decimals for representing
  ;; optional fractions. Add more if you ever need them.
  (time-secfrac
   (dot digit #'collapse-butfirst)
   (dot digit digit #'collapse-butfirst)
   (dot digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit digit digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit digit digit digit digit digit #'collapse-butfirst)
   (dot digit digit digit digit digit digit digit digit digit digit #'collapse-butfirst))

  (date-fullyear
   (digit digit digit digit #'collapse))

  (date-month
   (digit digit #'collapse))
                  
  (date-mday
   (digit digit #'collapse))

  (time-hour
   (digit digit #'collapse))

  (time-minute
   (digit digit #'collapse))

  (time-second
   (digit digit #'collapse)))

;; A lexer to pick the string apart and identify the different parts
;; for the parser.

(defun rfc3339-lexer (string)
  (let ((chars (coerce string 'list))
        (numbers (coerce "0123456789" 'list)))
    (lambda ()
      (let ((char (pop chars)))
        (when char
          (cond
            ((member char numbers :test #'char-equal)
             (values 'digit char))
            (t
             (case char
               (#\T 'big-t)
               (#\Space 'big-t) ; accept a space instead of T between date and time.
               (#\: 'colon)
               (#\+ (values 'plus #'+))
               (#\- (values 'minus #'-))
               (#\. 'dot)
               (#\Z 'zulu)
               (otherwise
                (error 'malformed-timestamp
                       :fault-string (format nil "I didnt-expect a ~(~a~) in a well-formed timestamp" (char-name char))))))))))))

;; Parse an RFC3339 formatted string using our custom lexer and yacc
;; parser.

(defun parse-string (string)
  (handler-case
      (parse-with-lexer (rfc3339-lexer string) *rfc3339-parser*)
    (yacc:yacc-parse-error ()
      (error 'malformed-timestamp :fault-string "You fed me a malformed RFC3339 timestamp"))))

(define-condition malformed-timestamp ()
  ((fault-string :initarg :fault-string :accessor fault-string-of)))
