(in-package #:zed)

(deftype image-pixel-format () '(member :red :rg :rgb :rgba))

(defstruct (image
            (:predicate nil)
            (:copier nil))
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (pixel-format :rgba :type image-pixel-format)
  (pixel-type :unsigned-byte :type keyword)
  (internal-format :rgba8 :type keyword)
  (data nil :type (or vector null)))

(u:define-printer (image stream :type nil)
  (format stream "IMAGE"))

(defun get-image-type (path)
  (u:make-keyword (string-upcase (pathname-type path))))

(defgeneric %load-image (type path)
  (:method (type path)
    (error "Unsupported image type ~s.~%~%Path: ~s" type path)))

(defgeneric load-image (asset &key &allow-other-keys))

(defmethod load-image ((asset null) &key width height pixel-format pixel-type internal-format)
  (make-image :width width
              :height height
              :pixel-format pixel-format
              :pixel-type pixel-type
              :internal-format internal-format))

(defmethod load-image (asset &key)
  (with-asset (asset path data)
    (destructuring-bind (asset-system asset-path) asset
      (v:info :zed "Loading image: ~a (~s)..." asset-path asset-system)
      (prog1 (%load-image (get-image-type path) data)
        (v:info :zed "Loaded image: ~a (~s)" asset-path asset-system)))))
