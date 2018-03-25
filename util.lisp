(in-package :gb)

(defclass promise ()
  ((closure
    :type function
    :accessor closure
    :initarg :closure)
   (return-type
    :accessor return-type
    :initarg :return-type
    :initform t))
  (:documentation "A delayed evaluation."))

(defclass u16-promise (promise)
   ((return-type
    :accessor return-type
    :initarg :return-type
    :initform '(unsigned-byte 16))))

(defclass s8-promise (promise)
   ((return-type
    :accessor return-type
    :initarg :return-type
    :initform '(signed-byte 8))))

(defmacro delay (expression &optional (return-type t))
  "Delay an evaluation."
  `(make-instance
    'promise
    :closure (lambda ()
	       ,expression)
    :return-type ,return-type))

(defmacro delay-u16 (expression)
  "Delay an evaluation that promises to evaluate as a 16-bit unsigned value."
  `(make-instance
    'u16-promise
    :closure (lambda ()
	       ,expression)))

(defmacro delay-s8 (expression)
  "Delay an evaluation that promises to evaluate as a 8-bit signed value."
  `(make-instance
    's8-promise
    :closure (lambda ()
	       ,expression)))

(defmethod force ((promise promise))
  "Force a delayed evaluation."
  (funcall (closure promise)))

(defun trunc-seq (seq max)
  "Truncate a sequence if it is longer than a maximum specified length."
  (if (> (length seq) max)
      (subseq seq 0 max)
      seq))
