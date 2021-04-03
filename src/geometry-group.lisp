(in-package #:zed)

(defstruct (geometry-group
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (format :interleaved :type keyword)
  (divisor 0 :type u:ub8)
  (attributes (u:dict #'eq) :type hash-table)
  (attribute-order nil :type list))

(defun make-geometry-groups (layout-data)
  (let ((groups (u:dict #'eq))
        (order nil))
    (dolist (group-data layout-data)
      (destructuring-bind (name (&key (format :interleaved) (divisor 0)) . attributes) group-data
        (u:mvlet* ((attributes attribute-order (make-geometry-attributes attributes))
                   (group (make-geometry-group
                           :name name
                           :format format
                           :divisor divisor
                           :attributes attributes
                           :attribute-order attribute-order)))
          (push name order)
          (setf (u:href groups name) group))))
    (values groups (nreverse order))))

(defun get-geometry-buffer-count (group)
  (ecase (geometry-group-format group)
    (:separate (hash-table-count (geometry-group-attributes group)))
    (:interleaved 1)))

(defun get-geometry-group-attribute-size (group)
  (reduce #'+ (u:hash-values (geometry-group-attributes group)) :key #'get-geometry-attribute-size))

(defun configure-geometry-group/separate (group index buffers)
  (loop :for attribute-name :in (geometry-group-attribute-order group)
        :for attribute = (u:href (geometry-group-attributes group) attribute-name)
        :for i :from index
        :for buffer :across buffers
        :for divisor = (geometry-group-divisor group)
        :do (gl:bind-buffer :array-buffer buffer)
            (configure-geometry-attribute attribute i 0 0 divisor)))

(defun configure-geometry-group/interleaved (group index buffers)
  (gl:bind-buffer :array-buffer (aref buffers 0))
  (loop :with stride = (get-geometry-group-attribute-size group)
        :with offset = 0
        :for attribute-name :in (geometry-group-attribute-order group)
        :for attribute = (u:href (geometry-group-attributes group) attribute-name)
        :for i :from index
        :for divisor = (geometry-group-divisor group)
        :do (configure-geometry-attribute attribute i stride offset divisor)
            (incf offset (get-geometry-attribute-size attribute))))

(defun configure-geometry-group (group index buffers)
  (ecase (geometry-group-format group)
    (:separate (configure-geometry-group/separate group index buffers))
    (:interleaved (configure-geometry-group/interleaved group index buffers))))
