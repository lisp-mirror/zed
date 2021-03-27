(in-package #:cl-user)

(defpackage #:%zed.image.hdr
  ;; Third-party aliases
  (:local-nicknames
   (#:ss #:split-sequence)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:img #:%zed.image))
  (:use #:cl)
  (:shadow
   #:position
   #:read-byte
   #:read-line
   #:stream))

(in-package #:%zed.image.hdr)

(defstruct (buffer
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  stream
  (position 0 :type u:ub32)
  (end 0 :type u:ub32)
  (data (u:make-ub8-array 0) :type u:ub8a))

(defun buffer-empty-p (buffer)
  (>= (position buffer) (end buffer)))

(defun refill-buffer (buffer)
  (when (buffer-empty-p buffer)
    (let ((end (read-sequence (data buffer) (stream buffer))))
      (setf (position buffer) 0
            (end buffer) end))))

(defun eof-p (buffer)
  (refill-buffer buffer)
  (buffer-empty-p buffer))

(declaim (inline read-hdr-image-byte))
(defun read-byte (buffer)
  (if (eof-p buffer)
      (cl:read-byte (stream buffer))
      (aref (data buffer) (1- (incf (position buffer))))))

(defun peek-byte (buffer)
  (if (eof-p buffer)
      (cl:read-byte (stream buffer))
      (aref (data buffer) (position buffer))))

(defun read-line (buffer)
  (let ((n nil)
        (next nil))
    (prog1 (babel:octets-to-string
            (coerce (loop :until (or (member (setf n (peek-byte buffer))
                                             '(10 13))
                                     (eof-p buffer))
                          :collect (read-byte buffer))
                    '(vector u:ub8)))
      (unless (eof-p buffer)
        (loop :do (read-byte buffer)
              :while (and (not (eof-p buffer))
                          (not (eql n (setf next (peek-byte buffer))))
                          (member next '(10 13))))))))

(defun read-header (buffer)
  (labels ((trim (string)
             (string-trim '(#\space #\newline #\tab) string))
           (parse-key (line)
             (when (and line
                        (string/= line "")
                        (char/= #\# (char line 0)))
               (let* ((delimiter (cl:position #\= line))
                      (key (u:make-keyword
                            (string-upcase (trim (subseq line 0 delimiter)))))
                      (value (trim (subseq line (1+ delimiter)))))
                 (when (eq key :format)
                   (let ((format (u:make-keyword
                                  (subseq (string-upcase value) 11 14))))
                     (unless (eq format :rgb)
                       (error "Unsupported HDR color space: ~a." format))
                     (list key format))))))
           (parse-xy (line)
             (destructuring-bind (axis1 dimension1 axis2 dimension2)
                 (ss:split-sequence #\space line :remove-empty-subseqs t)
               (unless (and (string= axis1 "-Y")
                            (string= axis2 "+X"))
                 (error "Unsupported HDR orientation: ~s." line))
               (list :width (parse-integer dimension2)
                     :height (parse-integer dimension1)))))
    (loop :for line = (read-line buffer)
          :for (k v) = (parse-key (trim line))
          :unless line
            :do (error "Invalid HDR header.")
          :until (zerop (length line))
          :if k
            :collect k :into kv
            :and
              :collect v :into kv
          :finally (return (append (parse-xy (read-line buffer)) kv)))))

(defun read-scanline (buffer length destination &key (offset 0))
  (declare (optimize speed)
           (fixnum length offset))
  (let ((stream (stream buffer))
        (data (data buffer))
        (pos (position buffer))
        (end (end buffer)))
    (declare ((u:ub32a (*)) destination)
             (u:ub24 pos end))
    (labels ((%read-byte ()
               (when (= pos end)
                 (setf pos 0
                       end (read-sequence data stream))
                 (when (zerop end)
                   (cl:read-byte stream)))
               (aref data (1- (incf pos))))
             (read-pixel ()
               (values (%read-byte) (%read-byte) (%read-byte) (%read-byte)))
             (old-rle-p (r g b e)
               (when (= r g b 1)
                 e))
             (new-rle-p (r g b e)
               (when (and (= r g 2) (< b 127))
                 (dpb b (byte 7 8) e)))
             (write-pixel (p r g b e)
               (let ((w 0))
                 (setf (ldb (byte 8 1) w) r
                       (ldb (byte 8 10) w) g
                       (ldb (byte 8 19) w) b
                       (ldb (byte 5 27) w) (min 31 (max 0 (- e 113))))
                 (setf (aref destination (+ offset p)) w)))
             (write-component (p c v)
               (let ((i (aref destination (+ offset p))))
                 (ecase c
                   (0 (setf (ldb (byte 8 1) i) v))
                   (1 (setf (ldb (byte 8 10) i) v))
                   (2 (setf (ldb (byte 8 19) i) v))
                   (3 (setf (ldb (byte 5 27) i) (min 31 (max 0 (- v 113)))))))))
      (declare (inline read-pixel old-rle-p new-rle-p write-pixel write-component))
      (loop :with p :of-type u:ub24 = 0
            :with rle = 0
            :with lr = 0
            :with lg = 0
            :with lb = 0
            :with le = 0
            :while (< p length)
            :do (u:mvlet ((r g b e (read-pixel)))
                  (cond
                    ((setf rle (old-rle-p r g b e))
                     (loop :repeat rle
                           :do (write-pixel p r g b e))
                     (incf p rle))
                    ((setf rle (new-rle-p r g b e))
                     (loop :for c :below 4
                           :for p2 :of-type u:ub16 = 0
                           :do (loop :for r2 = 0
                                     :while (< p2 rle)
                                     :do (setf r2 (%read-byte))
                                         (if (> r2 128)
                                             (loop :with v = (%read-byte)
                                                   :repeat (ldb (byte 7 0) r2)
                                                   :do (write-component p2 c v)
                                                       (incf p2))
                                             (loop :repeat r2
                                                   :do (write-component
                                                        p2 c (%read-byte))
                                                       (incf p2)))))
                     (incf p rle))
                    (t
                     (write-pixel p r g b e)
                     (incf p)))
                  (setf lr r lg g lb b le e)))
      (setf (position buffer) pos
            (end buffer) end))))

(defmethod img::%load ((type (eql :hdr)) stream)
  (let* ((data (u:make-ub8-array 8192))
         (buffer (make-buffer :stream stream :data data))
         (header (read-header buffer))
         (width (getf header :width))
         (height (getf header :height))
         (data (u:make-ub32-array (* width height) #xffffffff)))
    (loop :for y :below height
          :do (read-scanline buffer width data :offset (* y width)))
    (img::make-image :width width
                     :height height
                     :pixel-format :rgb
                     :pixel-type :unsigned-int-5-9-9-9-rev
                     :internal-format :rgb9-e5
                     :data data)))
