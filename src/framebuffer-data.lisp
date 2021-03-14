(in-package #:cl-user)

(defpackage #:%zed.framebuffer.data
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:win #:%zed.window))
  (:use #:cl)
  (:shadow
   #:find))

(in-package #:%zed.framebuffer.data)

(glob:define-global-var =data= (u:dict #'eq))

(deftype modes () '(member :read :write :read/write))

(defstruct (data
            (:constructor %make-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (mode :read/write :type modes)
  (attachments (u:dict #'eq) :type hash-table)
  (materials nil :type list))

(u:define-printer (data stream :type nil)
  (format stream "FRAMEBUFFER-DATA: ~s" (name data)))

(defstruct (attachment
            (:constructor %make-attachment)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (buffer nil :type list)
  (point nil :type list)
  (width nil :type function)
  (height nil :type function))

(u:define-printer (attachment stream :type nil)
  (format stream "FRAMEBUFFER-ATTACHMENT-DATA: ~s" (attachment-name attachment)))

(defun find (name)
  (or (u:href =data= name)
      (error "Framebuffer ~s is not defined." name)))

(defun make-attachment (data)
  (flet ((generate-size-func (dimension value)
           (lambda (window)
             (or value
                 (ecase dimension
                   (:width (win::width window))
                   (:height (win::height window)))))))
    (destructuring-bind (name &key point (buffer :render-buffer) width height) data
      (%make-attachment :name name
                        :buffer (u:ensure-list buffer)
                        :point (u:ensure-list point)
                        :width (generate-size-func :width width)
                        :height (generate-size-func :height height)))))

(defun find-attachment (data name)
  (u:href (attachments data) name))

(defun update (name mode attachments)
  (let* ((data (find name))
         (attachment-data (attachments data)))
    (setf (mode data) mode)
    (dolist (x attachments)
      (destructuring-bind (name &key &allow-other-keys) x
        (setf (u:href attachment-data name) (make-attachment x))))))

(defun make-data (name mode attachments)
  (let ((data (%make-data :name name)))
    (setf (u:href =data= name) data)
    (update name mode attachments)
    data))

(defmacro define-framebuffer (name (&key (mode :read/write)) &body body)
  `(if (u:href =data= ',name)
       (update ',name ',mode ',body)
       (make-data ',name ',mode ',body)))
