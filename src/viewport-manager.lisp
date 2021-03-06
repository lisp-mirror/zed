(in-package #:zed)

(defstruct (viewport-manager
            (:constructor %make-viewport-manager)
            (:predicate nil)
            (:copier nil))
  (window nil :type window)
  (table (u:dict #'eq) :type hash-table)
  (active nil :type (or viewport null))
  (default nil :type (or viewport null)))

(u:define-printer (viewport-manager stream :type nil)
  (format stream "VIEWPORT-MANAGER"))

(u:fn-> make-viewport-manager (window) viewport-manager)
(defun make-viewport-manager (window)
  (declare (optimize speed))
  (let* ((full-screen (make-viewport :full-screen window))
         (manager (%make-viewport-manager :window window)))
    (setf (u:href (viewport-manager-table manager) :full-screen) full-screen)
    manager))

(u:fn-> find-viewport (viewport-manager symbol) (or viewport null))
(declaim (inline find-viewport))
(defun find-viewport (manager name)
  (declare (optimize speed))
  (if (eq name :default)
      (or (viewport-manager-default manager)
          (values (u:href (viewport-manager-table manager) :full-screen)))
      (values (u:href (viewport-manager-table manager) name))))

(u:fn-> get-viewport-size (core &optional symbol) v2:vec)
(defun get-viewport-size (core &optional viewport-name)
  (let* ((manager (core-viewports core))
         (viewport (or (find-viewport manager viewport-name)
                       (viewport-manager-default manager))))
    (v2:vec (viewport-width viewport)
            (viewport-height viewport))))

(u:fn-> ensure-viewport (viewport-manager symbol trait) viewport)
(defun ensure-viewport (manager name camera)
  (declare (optimize speed))
  (unless (find-viewport manager name)
    (let ((viewport (make-viewport name (viewport-manager-window manager))))
      (setf (u:href (viewport-manager-table manager) name) viewport
            (viewport-camera viewport) camera)))
  (find-viewport manager name))

(u:fn-> find-viewport-by-coordinates (viewport-manager u:ub16 u:ub16) viewport)
(defun find-viewport-by-coordinates (manager x y)
  (declare (optimize speed))
  (u:do-hash-values (v (viewport-manager-table manager))
    (let ((vx (viewport-x v))
          (vy (viewport-y v))
          (vw (viewport-width v))
          (vh (viewport-height v)))
      (when (and (<= vx x (+ vx vw))
                 (<= vy y (+ vy vh)))
        (return-from find-viewport-by-coordinates v))))
  (viewport-manager-default manager))

(defmacro with-viewport ((core viewport) &body body)
  (u:with-gensyms (manager)
    `(let ((,manager (core-viewports ,core)))
       (unwind-protect
            (progn
              (configure-viewport ,viewport)
              (setf (viewport-manager-active ,manager) ,viewport)
              ,@body)
         (setf (viewport-manager-active ,manager) (viewport-manager-default ,manager))))))

(u:fn-> update-viewports (viewport-manager) null)
(defun update-viewports (manager)
  (declare (optimize speed))
  (u:do-hash-values (v (viewport-manager-table manager))
    (update-viewport v))
  nil)

(defmethod recompile ((type (eql :viewport)) data)
  (u:when-let ((viewport (find-viewport (core-viewports =core=) data)))
    (update-viewport viewport)
    (v:debug :zed "Recompiled viewport: ~s" data)))
