(in-package #:zed)

(deftype texture-source () '(or list (integer 1 #.(expt 2 16))))

(defstruct (texture-data
            (:constructor %make-texture-data)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (type nil :type symbol)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (pixel-format :rgba :type image-pixel-format)
  (pixel-type :unsigned-byte :type keyword)
  (internal-format :rgba8 :type keyword)
  (mipmaps-p t :type boolean)
  (parameters nil :type list)
  (source nil :type texture-source))

(u:define-printer (texture-data stream :type nil)
  (format stream "TEXTURE-DATA: ~s" (texture-data-name texture-data)))

(glob:define-global-var =textures= (u:dict #'eq))

(u:fn-> find-texture-data (symbol) texture-data)
(declaim (inline find-texture-data))
(defun find-texture-data (name)
  (declare (optimize speed))
  (or (u:href =textures= name)
      (error "Texture ~s is not defined." name)))

(defun update-texture-data (name type source width height pixel-format pixel-type internal-format
                            mipmaps-p parameters)
  (let ((data (find-texture-data name)))
    (setf (texture-data-type data) type
          (texture-data-width data) width
          (texture-data-height data) height
          (texture-data-pixel-format data) (or pixel-format :rgba)
          (texture-data-pixel-type data) (or pixel-type :unsigned-byte)
          (texture-data-internal-format data) (or internal-format :rgba8)
          (texture-data-mipmaps-p data) mipmaps-p
          (texture-data-parameters data) parameters
          (texture-data-source data) source)
    (when =context=
      (thread-pool-enqueue (list :texture name)))
    nil))

(defun make-texture-data (name &rest args)
  (let ((data (%make-texture-data :name name)))
    (setf (u:href =textures= name) data)
    (apply #'update-texture-data name args)
    data))

(defun make-texture-data-parameters (args)
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
    (let ((parameters (make-texture-data-parameters args)))
      `(if (u:href =textures= ',name)
           (update-texture-data ',name ,type ',source ,width ,height ,pixel-format ,pixel-type
                                ,internal-format ,mipmaps-p ',parameters)
           (make-texture-data ',name ,type ',source ,width ,height ,pixel-format ,pixel-type
                              ,internal-format ,mipmaps-p ',parameters)))))
