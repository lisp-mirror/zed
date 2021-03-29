(in-package #:cl-user)

(defpackage #:%zed.image
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:asset #:%zed.asset)
   (#:log #:%zed.logging))
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

(defgeneric load (asset &key &allow-other-keys))

(defmethod load ((asset null) &key width height pixel-format pixel-type internal-format)
  (make-image :width width
              :height height
              :pixel-format pixel-format
              :pixel-type pixel-type
              :internal-format internal-format))

(defmethod load (asset &key)
  (asset::with-asset (asset path data)
    (destructuring-bind (asset-system asset-path) asset
      (log::info :zed.image "Loading image: ~a (~s)..." asset-path asset-system)
      (prog1 (%load (get-type path) data)
        (log::info :zed.image "Loaded image: ~a (~s)" asset-path asset-system)))))
