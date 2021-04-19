(in-package #:zed)

(deftype framebuffer-modes () '(member :read :write :read/write))

(defstruct (framebuffer-data
            (:constructor %make-framebuffer-data)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (mode :read/write :type framebuffer-modes)
  (attachments (u:dict #'eq) :type hash-table)
  (materials nil :type list))

(u:define-printer (framebuffer-data stream :type nil)
  (format stream "FRAMEBUFFER-DATA: ~s" (framebuffer-data-name framebuffer-data)))

(defstruct (framebuffer-attachment
            (:constructor %make-framebuffer-attachment)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (buffer nil :type list)
  (point nil :type list)
  (width nil :type function)
  (height nil :type function))

(u:define-printer (framebuffer-attachment stream :type nil)
  (format stream "FRAMEBUFFER-ATTACHMENT: ~s" (framebuffer-attachment-name framebuffer-attachment)))

(glob:define-global-var =framebuffers= (u:dict #'eq))

(defun find-framebuffer-data (name)
  (or (u:href =framebuffers= name)
      (error "Framebuffer ~s is not defined." name)))

(defun make-framebuffer-attachment (data)
  (flet ((generate-size-func (dimension value)
           (lambda (window)
             (or value
                 (ecase dimension
                   (:width (window-width window))
                   (:height (window-height window)))))))
    (destructuring-bind (name &key point (buffer :render-buffer) width height) data
      (%make-framebuffer-attachment :name name
                                    :buffer (u:ensure-list buffer)
                                    :point (u:ensure-list point)
                                    :width (generate-size-func :width width)
                                    :height (generate-size-func :height height)))))

(defun find-framebuffer-attachment (data name)
  (u:href (framebuffer-data-attachments data) name))

(defun update-framebuffer-data (name mode attachments)
  (let* ((data (find-framebuffer-data name))
         (attachment-data (framebuffer-data-attachments data)))
    (setf (framebuffer-data-mode data) mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (setf (u:href attachment-data name) (make-framebuffer-attachment x))))))

(defun make-framebuffer-data (name mode attachments)
  (let ((data (%make-framebuffer-data :name name)))
    (setf (u:href =framebuffers= name) data)
    (update-framebuffer-data name mode attachments)
    data))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(if (u:href =framebuffers= ',name)
       (update-framebuffer-data ',name ',mode ',body)
       (make-framebuffer-data ',name ',mode ',body)))
