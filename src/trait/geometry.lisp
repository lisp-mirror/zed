(in-package #:zed.trait.geometry)

(z::define-internal-trait geometry ()
  ((%name :reader name
          :inline t
          :type symbol
          :initarg :name
          :initform nil)
   (%instance-count :reader instance-count
                    :inline t
                    :type (and (integer 1) fixnum)
                    :initarg :instance-count
                    :initform 1)
   (%cache-p :reader cache-p
             :inline t
             :type boolean
             :initarg :cache-p
             :initform t)
   (%resource :accessor resource
              :inline t
              :type (or z::geometry-data null)
              :initform nil)
   (%data :reader data
          :inline t
          :type hash-table
          :initform (u:dict #'eq))
   (%dirty-p :accessor dirty-p
             :inline t
             :type boolean
             :initform nil))
  (:setup setup)
  (:destroy destroy)
  (:render render))

;;; Hooks

(u:fn-> setup (geometry) null)
(defun setup (geometry)
  (declare (optimize speed))
  (let ((context (z:trait-context geometry))
        (name (name geometry)))
    (unless name
      (error "Geometry trait ~s must have a name specified." geometry))
    (let ((resource (if (cache-p geometry)
                        (z::with-resource-cache (context :geometry name)
                          (prog1 (z::make-geometry name)
                            (v:debug :zed "Cached geometry: ~s" name)))
                        (z::make-geometry name))))
      (setf (resource geometry) resource)
      nil)))

(u:fn-> render (geometry) null)
(defun render (geometry)
  (declare (optimize speed))
  (let ((resource (resource geometry)))
    (when (dirty-p geometry)
      (let ((data (data geometry)))
        (u:do-hash (k v data)
          (z::update-geometry resource k v))
        (clrhash data)
        (setf (dirty-p geometry) nil)))
    (z::draw-geometry (resource geometry) (instance-count geometry))
    nil))

(u:fn-> destroy (geometry) null)
(defun destroy (geometry)
  (declare (optimize speed))
  (u:when-let ((resource (resource geometry)))
    (unless (cache-p geometry)
      (z::delete-geometry resource)))
  nil)
