(in-package #:cl-user)

(defpackage #:%zed.binary-parser
  ;; Third-party aliases
  (:local-nicknames
   (#:io #:fast-io))
  (:use #:cl))

(in-package #:%zed.binary-parser)

(defun get-string-length (buffer byte-count null-terminated-p)
  (let* ((sequence (io:input-buffer-vector buffer))
         (max-length (or byte-count (length sequence)))
         (start (io:buffer-position buffer))
         (end (min (length sequence) (+ start max-length)))
         (index (if null-terminated-p
                    (position 0 sequence :start start :end end)
                    end)))
    (- index start)))

(defun octets= (octets1 octets2)
  (equalp octets1 (io:octets-from octets2)))

(defun parse-bytes (buffer count)
  (let ((octet-vector (io:make-octet-vector count)))
    (io:fast-read-sequence octet-vector buffer)
    octet-vector))

(defun parse-uint/be (buffer byte-count)
  (loop :with value = 0
        :for i :from (* (1- byte-count) 8) :downto 0 :by 8
        :for byte = (io:fast-read-byte buffer)
        :do (setf (ldb (byte 8 i) value) byte)
        :finally (return value)))

(defun parse-uint/le (buffer byte-count)
  (loop :with value = 0
        :for i :below (* byte-count 8) :by 8
        :for byte = (io:fast-read-byte buffer)
        :do (setf (ldb (byte 8 i) value) byte)
        :finally (return value)))

(defun parse-int/be (buffer byte-count)
  (let ((value (parse-uint/be buffer byte-count))
        (size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun parse-int/le (buffer byte-count)
  (let* ((value (parse-uint/le buffer byte-count))
         (size (* byte-count 8)))
    (logior (* (ldb (byte 1 (1- size)) value)
               (- (expt 2 size)))
            value)))

(defun parse-string (buffer &key byte-count (encoding :ascii) null-p)
  (let ((octet-vector (io:make-octet-vector (get-string-length buffer byte-count null-p))))
    (io:fast-read-sequence octet-vector buffer)
    (when null-p
      (io:fast-read-byte buffer))
    (babel:octets-to-string octet-vector :encoding encoding)))
