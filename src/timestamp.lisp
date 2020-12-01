(in-package #:rfc3339)

(defvar *default-offset* (- 0 (* (nth-value 8 (get-decoded-time)) 3600)))
  
(defclass timestamp ()
  ((utc-time
    :initarg :utc-time
    :accessor utc-time-of
    :documentation "The timestamp as a universal time value with the
   accuracy of on second. This should be in UTC and the offset should
   go in the offset slot.")
   (fraction
    :initform 0
    :initarg :fraction
    :accessor fraction-of
    :documentation "Represents the fractions that can not be
    represented using whole seconds of universal time.")
   (offset
    :initarg :offset
    :accessor offset-of
    :documentation "Represent the offset in seconds relative to
    UTC. UTC+1 should be an offset of -3600")))

(defun time-string (time &optional stream)
  (multiple-value-bind (second minute hour) (decode-universal-time time)
    (format stream "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun date-string (time &optional stream)
  (multiple-value-bind (s m h day month year) (decode-universal-time time)
    (declare (ignore s m h))
    (format stream "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun make-timestamp (&key (utc-time (get-universal-time)) (fraction 0) (offset *default-offset*))
  "When supplying utc-time you must make sure that you also supply a
correct offset. For UTC+00:00 the offset is 0. For UTC-01:00 the
offset would be -3600 and for UTC+01:00 the offset would be 3600.

Should you supply an offset or set *default-offset* to a value which
does not translates to whole hours and seconds and thus can not be
represented using a time offset of just hours and minutes we will
break horribly so that you notice it."
  (assert (= (nth-value 1 (truncate (nth-value 1 (truncate offset 3600)) 60)) 0))
  (make-instance 'timestamp :utc-time (- utc-time offset) :fraction fraction :offset offset))

(defmethod local-time-of ((timestamp timestamp))
  (+ (utc-time-of timestamp)
     (offset-of timestamp)))

(defmethod utc-date-string ((timestamp timestamp) &optional stream)
  (date-string (utc-time-of timestamp) stream))

(defmethod utc-time-string ((timestamp timestamp) &optional stream)
  (time-string (utc-time-of timestamp) stream))

(defmethod local-date-string ((timestamp timestamp) &optional stream)
  (date-string (local-time-of timestamp) stream))

(defmethod local-time-string ((timestamp timestamp) &optional stream)
  (time-string (local-time-of timestamp) stream))

(defmethod rfc3339-timestamp ((timestamp timestamp) &optional stream)
  "This is the timestamp such as it should look according to
RFC3339. Unfortunately it doesn't work for xml-rpc which uses someones
ugly arbitrary idea of iso8601 and thus doesn't allow for fractions or
timezones."
  (let ((string (with-output-to-string (str)
                  (multiple-value-bind (second minute hour day month year) (decode-universal-time (utc-time-of timestamp))
                    (format str "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~d" year month day hour minute second (fraction-of timestamp)))
                  (if (not (= (offset-of timestamp) 0))
                      (multiple-value-bind (whole-hours rem-seconds) (truncate (offset-of timestamp) 3600)
                        (if (> (offset-of timestamp) 0)
                            (princ #\+ str)
                            (princ #\- str))
                        (format str "~2,'0d:~2,'0d" (abs whole-hours) (abs (truncate rem-seconds 60))))
                      (princ #\Z str)))))
    (when stream
      (princ string stream))
    string))

(defmethod xml-rpc-timestamp ((timestamp timestamp) &optional stream)
  "This is the version of iso8601 that is used at xmlrpc.com/spec and
that ruby seems to like. We use local-time because when using xml-rpc
the time zone should be part of the api documentation rather than the
timestamp which is a bit limiting..."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (local-time-of timestamp))
    (format stream "~4,'0d~2,'0d~2,'0dT~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

(defmethod print-object ((timestamp timestamp) stream)
  (rfc3339-timestamp timestamp stream))
