(in-package #:cl-user)

(defpackage #:%zed.geometry.attribute
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl)
  (:shadow
   #:type))

(in-package #:%zed.geometry.attribute)

(defstruct (attribute
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (normalize nil :type boolean)
  (type :float :type keyword)
  (out-type :float :type keyword)
  (element-count 0 :type u:ub32))

(defun make-attributes (spec)
  (let ((attributes (u:dict #'eq))
        (order nil))
    (dolist (attribute spec)
      (destructuring-bind (name &key normalize (type :float) (out-type type) (count 1)) attribute
        (push name order)
        (setf (u:href attributes name)
              (make-attribute :name name
                              :normalize normalize
                              :type (u:make-keyword type)
                              :out-type (u:make-keyword out-type)
                              :element-count count))))
    (values attributes (nreverse order))))

(defun get-size (attribute)
  (* (element-count attribute)
     (ecase (type attribute)
       ((:byte :unsigned-byte) 1)
       ((:short :unsigned-short :half-float) 2)
       ((:int :unsigned-int :float :fixed) 4)
       (:double 8))))

(defun configure (attribute index stride offset divisor)
  (let ((normalize (if (normalize attribute) 1 0)))
    (ecase (type attribute)
      ((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int)
       (%gl:vertex-attrib-ipointer index
                                   (element-count attribute)
                                   (type attribute)
                                   stride
                                   offset))
      ((:half-float :float :fixed)
       (%gl:vertex-attrib-pointer index
                                  (element-count attribute)
                                  (type attribute)
                                  normalize
                                  stride
                                  offset))
      (:double
       (%gl:vertex-attrib-lpointer index
                                   (element-count attribute)
                                   (type attribute)
                                   stride
                                   offset)))
    (%gl:vertex-attrib-divisor index divisor)))
