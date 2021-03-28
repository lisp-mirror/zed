(in-package #:cl-user)

(defpackage #:%zed.geometry.group
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:geo.attr #:%zed.geometry.attribute))
  (:use #:cl)
  (:shadow
   #:format))

(in-package #:%zed.geometry.group)

(defstruct (group
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (format :interleaved :type keyword)
  (divisor 0 :type u:ub8)
  (attributes (u:dict #'eq) :type hash-table)
  (attribute-order nil :type list))

(defun make-groups (layout-spec)
  (let ((groups (u:dict #'eq))
        (order nil))
    (dolist (group-spec layout-spec)
      (destructuring-bind (name (&key (format :interleaved) (divisor 0)) . attributes) group-spec
        (u:mvlet* ((attributes attribute-order (geo.attr::make-attributes attributes))
                   (group (make-group
                           :name name
                           :format format
                           :divisor divisor
                           :attributes attributes
                           :attribute-order attribute-order)))
          (push name order)
          (setf (u:href groups name) group))))
    (values groups (nreverse order))))

(defun get-buffer-count (group)
  (ecase (format group)
    (:separate (hash-table-count (attributes group)))
    (:interleaved 1)))

(defun get-attribute-size (group)
  (reduce #'+ (u:hash-values (attributes group)) :key #'geo.attr::get-size))

(defun configure/separate (group index buffers)
  (loop :for attribute-name :in (attribute-order group)
        :for attribute = (u:href (attributes group) attribute-name)
        :for i :from index
        :for buffer :across buffers
        :for divisor = (divisor group)
        :do (gl:bind-buffer :array-buffer buffer)
            (geo.attr::configure attribute i 0 0 divisor)))

(defun configure/interleaved (group index buffers)
  (gl:bind-buffer :array-buffer (aref buffers 0))
  (loop :with stride = (get-attribute-size group)
        :with offset = 0
        :for attribute-name :in (attribute-order group)
        :for attribute = (u:href (attributes group) attribute-name)
        :for i :from index
        :for divisor = (divisor group)
        :do (geo.attr::configure attribute i stride offset divisor)
            (incf offset (geo.attr::get-size attribute))))

(defun configure (group index buffers)
  (ecase (format group)
    (:separate (configure/separate group index buffers))
    (:interleaved (configure/interleaved group index buffers))))
