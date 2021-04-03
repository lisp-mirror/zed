(in-package #:zed)

(defstruct (geometry-attribute
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (normalize-p nil :type boolean)
  (type :float :type keyword)
  (out-type :float :type keyword)
  (element-count 0 :type u:ub32))

(defun make-geometry-attributes (data)
  (let ((attributes (u:dict #'eq))
        (order nil))
    (dolist (attribute data)
      (destructuring-bind (name &key normalize-p (type :float) (out-type type) (count 1)) attribute
        (push name order)
        (setf (u:href attributes name)
              (make-geometry-attribute :name name
                                       :normalize-p normalize-p
                                       :type (u:make-keyword type)
                                       :out-type (u:make-keyword out-type)
                                       :element-count count))))
    (values attributes (nreverse order))))

(defun get-geometry-attribute-size (attribute)
  (* (geometry-attribute-element-count attribute)
     (ecase (geometry-attribute-type attribute)
       ((:byte :unsigned-byte) 1)
       ((:short :unsigned-short :half-float) 2)
       ((:int :unsigned-int :float :fixed) 4)
       (:double 8))))

(defun configure-geometry-attribute (attribute index stride offset divisor)
  (let ((normalize (if (geometry-attribute-normalize-p attribute) 1 0)))
    (ecase (geometry-attribute-type attribute)
      ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
       (%gl:vertex-attrib-ipointer index
                                   (geometry-attribute-element-count attribute)
                                   (geometry-attribute-type attribute)
                                   stride
                                   offset))
      ((:half-float :float :fixed)
       (%gl:vertex-attrib-pointer index
                                  (geometry-attribute-element-count attribute)
                                  (geometry-attribute-type attribute)
                                  normalize
                                  stride
                                  offset))
      (:double
       (%gl:vertex-attrib-lpointer index
                                   (geometry-attribute-element-count attribute)
                                   (geometry-attribute-type attribute)
                                   stride
                                   offset)))
    (%gl:vertex-attrib-divisor index divisor)))
