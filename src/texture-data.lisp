(in-package #:cl-user)

(defpackage #:%zed.texture.data
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:img #:%zed.image)
   (#:tp #:%zed.thread-pool)
   (#:util #:%zed.util)
   (#:v4 #:zed.math.vector4))
  (:use #:cl)
  (:shadow
   #:find
   #:type)
  (:export
   #:define-texture))

(in-package #:%zed.texture.data)

(glob:define-global-var =data= (u:dict #'eq))

(deftype source () '(or list (integer 1 #.(expt 2 16))))

(defstruct (data
            (:constructor %make-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (type nil :type symbol)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (pixel-format :rgba :type img::pixel-format)
  (pixel-type :unsigned-byte :type keyword)
  (internal-format :rgba8 :type keyword)
  (mipmaps-p t :type boolean)
  (parameters nil :type list)
  (source nil :type source))

(u:define-printer (data stream :type nil)
  (format stream "TEXTURE-DATA: ~s" (name data)))

(u:fn-> find (symbol) data)
(declaim (inline find))
(defun find (name)
  (declare (optimize speed))
  (or (u:href =data= name)
      (error "Texture ~s is not defined." name)))

(defun update (name type source width height pixel-format pixel-type internal-format mipmaps-p
               parameters)
  (let ((data (find name)))
    (setf (type data) type
          (width data) width
          (height data) height
          (pixel-format data) (or pixel-format :rgba)
          (pixel-type data) (or pixel-type :unsigned-byte)
          (internal-format data) (or internal-format :rgba8)
          (mipmaps-p data) mipmaps-p
          (parameters data) parameters
          (source data) source)
    (when util::=context=
      (tp::enqueue (list :texture name)))
    nil))

(defun make-data (name &rest args)
  (let ((data (%make-data :name name)))
    (setf (u:href =data= name) data)
    (apply #'update name args)
    data))

(defun make-parameters (args)
  (destructuring-bind (&key
                         (depth-stencil-mode :depth-component)
                         (base-level 0)
                         (border-color (v4:zero))
                         (compare-func :lequal)
                         (compare-mode :none)
                         (lod-bias 0.0)
                         (min-filter :linear-mipmap-linear)
                         (mag-filter :linear)
                         (min-lod -1000)
                         (max-lod 1000)
                         (max-level 1000)
                         (swizzle-r :red)
                         (swizzle-g :green)
                         (swizzle-b :blue)
                         (swizzle-a :alpha)
                         (wrap-s :repeat)
                         (wrap-t :repeat)
                         (wrap-r :repeat)
                       &allow-other-keys)
      args
    (list :depth-stencil-texture-mode depth-stencil-mode
          :texture-base-level base-level
          :texture-border-color border-color
          :texture-compare-func compare-func
          :texture-compare-mode compare-mode
          :texture-lod-bias lod-bias
          :texture-min-filter min-filter
          :texture-mag-filter mag-filter
          :texture-min-lod min-lod
          :texture-max-lod max-lod
          :texture-max-level max-level
          :texture-swizzle-r swizzle-r
          :texture-swizzle-g swizzle-g
          :texture-swizzle-b swizzle-b
          :texture-swizzle-a swizzle-a
          :texture-wrap-s wrap-s
          :texture-wrap-t wrap-t
          :texture-wrap-r wrap-r)))

(defmacro define-texture (name (&optional (type :2d)) &body body)
  (destructuring-bind (&rest args
                       &key source width height (mipmaps-p t) pixel-format pixel-type
                         internal-format
                       &allow-other-keys)
      (car body)
    (let ((parameters (make-parameters args)))
      `(if (u:href =data= ',name)
           (update ',name ,type ',source ,width ,height ,pixel-format ,pixel-type ,internal-format
                   ,mipmaps-p ',parameters)
           (make-data ',name ,type ',source ,width ,height ,pixel-format ,pixel-type
                      ,internal-format ,mipmaps-p ',parameters)))))
