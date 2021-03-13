(in-package #:cl-user)

(defpackage #:%zed.texture
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils)
   (#:v4 #:origin.vec4))
  ;; Internal aliases
  (:local-nicknames
   (#:asset #:%zed.asset-pool)
   (#:ctx #:%zed.context)
   (#:data #:%zed.texture.data)
   (#:img #:%zed.image))
  (:use #:cl)
  (:shadow
   #:load))

(in-package #:%zed.texture)

(defstruct (texture
            (:constructor %make-texture)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; TODO: maybe (or null) like Z
  (data nil :type data::texture)
  (target :texture-2d :type keyword)
  (id 0 :type u:ub16)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (materials nil :type list))

(u:define-printer (texture stream :type nil)
  (format stream "TEXTURE: ~s" (data::name (data texture))))

(u:fn-> calculate-mipmap-levels (data::texture u:ub16 u:ub16) u:ub8)
(defun calculate-mipmap-levels (data width height)
  (declare (optimize speed))
  (if (data::mipmaps-p data)
      (loop :for i :of-type fixnum :from 0
            :while (or (> (ash width (- i)) 1)
                       (> (ash height (- i)) 1))
            :finally (return i))
      1))

(u:fn-> bind (texture u:ub8) null)
(declaim (inline bind))
(defun bind (texture unit)
  (declare (optimize speed))
  (gl:active-texture unit)
  (gl:bind-texture (target texture) (id texture)))

(u:fn-> configure (texture) null)
(defun configure (texture)
  (declare (optimize speed))
  (let ((id (id texture))
        (target (target texture))
        (data (data texture)))
    (gl:bind-texture target id)
    (when (data::mipmaps-p data)
      (gl:generate-mipmap target))
    (u:do-plist (k v (data::parameters data))
      (gl:tex-parameter target k v))
    (gl:bind-texture target 0)
    nil))

(u:fn-> load-framebuffer-texture (data::texture u:ub16 u:ub16) img::image)
(declaim (inline load-framebuffer-texture))
(defun load-framebuffer-texture (data width height)
  (declare (optimize speed))
  (values
   (img::load nil
              :width width
              :height height
              :pixel-format (data::pixel-format data)
              :pixel-type (data::pixel-type data)
              :internal-format (data::internal-format data))))

(defgeneric update (type texture source))

(defgeneric load-source (data type source &key &allow-other-keys)
  (:method :around (data type source &key)
    (declare (optimize speed))
    (let* ((loaded (call-next-method))
           (source-list (u:flatten (u:ensure-list loaded))))
      (unless (and (every #'img::width source-list)
                   (every #'img::height source-list))
        (error "Texture ~s does not have a width and height set."
               (data::name data)))
      loaded)))

(u:fn-> make-target (keyword) keyword)
(declaim (inline make-target))
(defun make-target (type)
  (declare (optimize speed))
  (values (u:format-symbol :keyword "TEXTURE-~a" type)))

(u:fn-> make-texture (data::texture keyword data::source) texture)
(defun make-texture (data type source)
  (declare (optimize speed))
  (let ((texture (%make-texture :data data :target (make-target type))))
    (update type texture source)
    texture))

(u:fn-> load
        (ctx::context symbol &key (:width (or u:ub16 null)) (:height (or u:ub16 null)))
        texture)
(defun load (context name &key width height)
  (asset::with-asset-cache (context :texture name)
      (let* ((data (data::find name))
             (type (data::type data))
             (source (load-source data type (data::source data) :width width :height height))
             (texture (make-texture data type source)))
        (configure texture)
        texture)))
