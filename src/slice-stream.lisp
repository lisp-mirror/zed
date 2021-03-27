(in-package #:cl-user)

(defpackage #:%zed.slice-stream
  ;; Third-party aliases
  (:local-nicknames
   (#:tgs #:trivial-gray-streams)
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:length
   #:stream))

(in-package #:%zed.slice-stream)

(defclass slice-stream (tgs:fundamental-binary-input-stream)
  ((%stream :accessor stream
            :initarg :stream)
   (%length :reader length
            :initarg :length)
   (%counter :accessor counter
             :initform 0)))

(defun make-slice-stream (stream offset length)
  (make-instance 'slice-stream :stream stream :offset offset :length length))

(defmethod initialize-instance :after ((object slice-stream) &key stream offset)
  (file-position stream offset))

(defmethod tgs:stream-read-byte ((stream slice-stream))
  (if (<= (length stream) (counter stream))
      :eof
      (prog1 (read-byte (stream stream) nil :eof)
        (incf (counter stream)))))

(defmethod tgs:stream-read-sequence ((stream slice-stream) sequence start end &key)
  (let ((end (when end (min end (- (length stream) (counter stream))))))
    (read-sequence sequence (stream stream) :start (or start 0) :end end)))

(defmethod tgs:stream-listen ((stream slice-stream))
  (listen (stream stream)))

(defmethod close ((stream slice-stream) &key abort)
  (close (stream stream) abort))

(defmethod open-stream-p ((stream slice-stream))
  (open-stream-p (stream stream)))

(defmethod stream-element-type ((stream slice-stream))
  (stream-element-type (stream stream)))
