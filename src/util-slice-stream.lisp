(in-package #:%zed.utility.stream-slice)

(defclass stream-slice (tgs:fundamental-binary-input-stream)
  ((%stream :accessor stream
            :initarg :stream)
   (%length :reader length
            :initarg :length)
   (%counter :accessor counter
             :initform 0)))

(defun make-stream-slice (stream offset length)
  (make-instance 'stream-slice :stream stream :offset offset :length length))

(defmethod initialize-instance :after ((object stream-slice) &key stream offset)
  (file-position stream offset))

(defmethod tgs:stream-read-byte ((stream stream-slice))
  (if (<= (length stream) (counter stream))
      :eof
      (prog1 (read-byte (stream stream) nil :eof)
        (incf (counter stream)))))

(defmethod tgs:stream-read-sequence ((stream stream-slice) sequence start end &key)
  (let ((end (when end (min end (- (length stream) (counter stream))))))
    (read-sequence sequence (stream stream) :start (or start 0) :end end)))

(defmethod tgs:stream-listen ((stream stream-slice))
  (listen (stream stream)))

(defmethod close ((stream stream-slice) &key abort)
  (close (stream stream) abort))

(defmethod open-stream-p ((stream stream-slice))
  (open-stream-p (stream stream)))

(defmethod stream-element-type ((stream stream-slice))
  (stream-element-type (stream stream)))
