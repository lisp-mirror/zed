(in-package #:cl-user)

(defpackage #:%zed.image
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:asset #:%zed.asset))
  (:use #:cl)
  (:shadow
   #:load))

(in-package #:%zed.image)

(deftype pixel-format () '(member :red :rg :rgb :rgba))

(defstruct (image
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (pixel-format :rgba :type pixel-format)
  (pixel-type :unsigned-byte :type keyword)
  (internal-format :rgba8 :type keyword)
  (data nil :type (or vector null)))

(u:define-printer (image stream :type nil)
  (format stream "IMAGE"))

(defun get-type (path)
  (u:make-keyword (string-upcase (pathname-type path))))

(defgeneric %load (type path)
  (:method (type path)
    (error "Unsupported image type ~s.~%~%Path: ~s" type path)))

(defgeneric load (context asset &key &allow-other-keys))

(defmethod load (context (asset null) &key width height pixel-format pixel-type internal-format)
  (make-image :width width
              :height height
              :pixel-format pixel-format
              :pixel-type pixel-type
              :internal-format internal-format))

(defmethod load (context asset &key)
  (asset::with-asset (asset path data)
    (%load (get-type path) data)))
