(in-package #:cl-user)

(defpackage #:%zed.geometry.layout.data
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:geo.group #:%zed.geometry.group))
  (:use #:cl)
  (:shadow
   #:find)
  (:export
   #:define-geometry-layout))

(in-package #:%zed.geometry.layout.data)

(glob:define-global-var =data= (u:dict #'eq))

(defstruct (data
            (:constructor %make-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (groups (u:dict #'eq) :type hash-table)
  (group-order nil :type list))

(defun find (name)
  (or (u:href =data= name)
      (error "Geometry layout ~s not found." name)))

(defun update (name groups order)
  (let ((layout (u:href =data= name)))
    (setf (groups layout) groups
          (group-order layout) order)))

(defun make-data (name groups order)
  (let ((layout (%make-data :name name)))
    (setf (u:href =data= name) layout)
    (update name groups order)))

(defmacro define-geometry-layout (name options &body body)
  (declare (ignore options))
  (u:with-gensyms (groups order)
    `(u:mvlet ((,groups ,order (geo.group::make-groups ',body)))
       (if (u:href =data= ',name)
           (update ',name ,groups ,order)
           (make-data ',name ,groups ,order)))))
