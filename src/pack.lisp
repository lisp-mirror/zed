(in-package #:cl-user)

(defpackage #:%zed.pack
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:io #:fast-io)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:bin #:%zed.binary-parser)
   (#:ss #:%zed.slice-stream)
   (#:util #:%zed.util))
  (:use #:cl))

(in-package #:%zed.pack)

(glob:define-global-var =index= (u:dict #'equalp))

(u:define-constant +magic-bytes+
    (make-array 8 :element-type 'u:ub8 :initial-contents '(90 101 100 0 80 97 99 107))
  :test #'equalp)

(defun collect-files (system)
  (let ((base (asdf:system-relative-pathname system "data/"))
        (results nil))
    (u:map-files base
                 (lambda (x)
                   (push (cons (namestring x)
                               (uiop/common-lisp:enough-namestring x base))
                         results)))
    (nreverse results)))

(defun write-uint (buffer integer size)
  (loop :with value = 0
        :for i :from (* (1- size) 8) :downto 0 :by 8
        :for byte = (ldb (byte 8 i) integer)
        :do (io:fast-write-byte byte buffer)))

(defun read-file-data (path)
  (u:with-binary-input (in path)
    (let ((buffer (io:make-input-buffer :stream in))
          (octets (io:make-octet-vector (file-length in))))
      (io:fast-read-sequence octets buffer)
      octets)))

(defun write-files (buffer system)
  (dolist (file (collect-files system))
    (destructuring-bind (full-path . path) file
      (let ((data (read-file-data full-path))
            (path-bytes (babel:string-to-octets (format nil "/~s/~a" system path)
                                                :encoding :utf-8)))
        (write-uint buffer (+ (length path-bytes) (length data) 5) 4)
        (io:fast-write-sequence path-bytes buffer)
        (io:fast-write-byte 0 buffer)
        (io:fast-write-sequence data buffer)
        (format t "Packed file: ~a (~(~a~))~%" path system)))))

(defun get-pack-file ()
  (format nil "~(~a~).zpk" util::=system-name=))

(defun get-pack-path ()
  #+zed.release
  (uiop:merge-pathnames* (get-pack-file)
                         (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*))))

(defun make-pack (&key path systems)
  (let ((path (uiop:merge-pathnames* (get-pack-file) path))
        (buffer (io:make-output-buffer)))
    (format t "Building pack file...~%")
    (u:with-binary-output (out path)
      (io:fast-write-sequence +magic-bytes+ buffer)
      (dolist (system (union '(:zed) systems))
        (write-files buffer system))
      (write-sequence (io:finish-output-buffer buffer) out))
    (format t "Pack file written: ~a~%" (namestring path))))

(defun decode-file-metadata (buffer)
  (let* ((length (bin::parse-uint/be buffer 4))
         (file-buffer (io:make-input-buffer :vector (bin::parse-bytes buffer (- length 4))))
         (path (bin::parse-string file-buffer :encoding :utf-8 :null-p t))
         (data-size (- length 5 (length (babel:string-to-octets path))))
         (data-offset (- (io:buffer-position buffer) data-size)))
    (bin::parse-bytes file-buffer data-size)
    (values path
            (cons data-offset data-size))))

(defun read-pack ()
  #+zed.release
  (let ((path (get-pack-path)))
    (u:with-binary-input (in path)
      (let ((buffer (io:make-input-buffer :stream in)))
        (unless (bin::octets= (bin::parse-bytes buffer 8) +magic-bytes+)
          (error "Corrupt pack file: ~s" path))
        (loop :with stream = (io:input-buffer-stream buffer)
              :until (= (file-position in) (file-length in))
              :do (u:mvlet ((path entry (decode-file-metadata buffer)))
                    (setf (u:href =index= path) entry)))))))

(defun read-binary-string (stream)
  (let ((bytes (u:make-ub8-array (ss::length stream))))
    (read-sequence bytes stream)
    (babel:octets-to-string bytes :encoding :utf-8)))

(defmacro with-pack-file ((data path &key (format :binary)) &body body)
  (u:with-gensyms (pack-stream offset size stream)
    `(destructuring-bind (,offset . ,size) (u:href =index= ,path)
       (u:with-binary-input (,pack-stream (get-pack-path))
         (let* ((,stream (ss::make-slice-stream ,pack-stream ,offset ,size))
                (,data ,(ecase format
                          (:binary stream)
                          (:string `(read-binary-string ,stream))
                          (:lisp `(read-from-string (read-binary-string ,stream))))))
           ,@body)))))
