(in-package #:zed)

(defstruct (texture
            (:constructor %make-texture)
            (:predicate nil)
            (:copier nil))
  (data nil :type (or texture-data null))
  (target :texture-2d :type keyword)
  (id 0 :type u:ub16)
  (width nil :type (or u:ub16 null))
  (height nil :type (or u:ub16 null))
  (materials nil :type list))

(u:define-printer (texture stream :type nil)
  (format stream "TEXTURE: ~a"
          (u:if-let ((data (texture-data texture)))
            (texture-data-name data)
            "(no data)")))

(u:fn-> calculate-texture-mipmap-levels (texture-data u:ub16 u:ub16) u:ub8)
(defun calculate-texture-mipmap-levels (data width height)
  (declare (optimize speed))
  (if (texture-data-mipmaps-p data)
      (loop :for i :of-type fixnum :from 0
            :while (or (> (ash width (- i)) 1)
                       (> (ash height (- i)) 1))
            :finally (return i))
      1))

(u:fn-> bind-texture (texture u:ub8) null)
(declaim (inline bind-texture))
(defun bind-texture (texture unit)
  (declare (optimize speed))
  (gl:active-texture unit)
  (gl:bind-texture (texture-target texture) (texture-id texture)))

(u:fn-> configure-texture (texture) null)
(defun configure-texture (texture)
  (declare (optimize speed))
  (let ((id (texture-id texture))
        (target (texture-target texture))
        (data (texture-data texture)))
    (gl:bind-texture target id)
    (when (texture-data-mipmaps-p data)
      (gl:generate-mipmap target))
    (u:do-plist (k v (texture-data-parameters data))
      (gl:tex-parameter target k v))
    (gl:bind-texture target 0)
    nil))

(u:fn-> load-framebuffer-texture (texture-data u:ub16 u:ub16) image)
(declaim (inline load-framebuffer-texture))
(defun load-framebuffer-texture (data width height)
  (declare (optimize speed))
  (values
   (load-image nil
               :width width
               :height height
               :pixel-format (texture-data-pixel-format data)
               :pixel-type (texture-data-pixel-type data)
               :internal-format (texture-data-internal-format data))))

(defgeneric update-texture (type texture source))

(defgeneric load-texture-source (data type source &key &allow-other-keys)
  (:method :around (data type source &key)
    (declare (optimize speed))
    (let* ((loaded (call-next-method))
           (source-list (u:flatten (u:ensure-list loaded))))
      (unless (and (every #'image-width source-list)
                   (every #'image-height source-list))
        (error "Texture ~s does not have a width and height set." (texture-data-name data)))
      loaded)))

(u:fn-> make-texture-target (keyword) keyword)
(declaim (inline make-texture-target))
(defun make-texture-target (type)
  (declare (optimize speed))
  (values (u:format-symbol :keyword "TEXTURE-~a" type)))

(u:fn-> make-texture (texture-data keyword (or image list)) texture)
(defun make-texture (data type source)
  (declare (optimize speed))
  (let ((texture (%make-texture :data data :target (make-texture-target type))))
    (update-texture type texture source)
    texture))

(u:fn-> load-texture
        (context symbol &key (:width (or u:ub16 null)) (:height (or u:ub16 null)))
        texture)
(defun load-texture (context name &key width height)
  (with-resource-cache (context :texture name)
    (v:debug :zed "Loading texture: ~s..." name)
    (let* ((data (find-texture-data name))
           (type (texture-data-type data))
           (source (load-texture-source data
                                        type
                                        (texture-data-source data)
                                        :width width
                                        :height height))
           (texture (make-texture data type source)))
      (configure-texture texture)
      (v:debug :zed "Texture loaded: ~s" name)
      texture)))

(defmethod recompile ((type (eql :texture)) data)
  (u:when-let ((texture (find-resource =context= :texture data)))
    (gl:delete-texture (texture-id texture))
    (delete-resource =context= :texture data)
    (load-texture =context=
                  data
                  :width (texture-width texture)
                  :height (texture-height texture))
    (dolist (material-name (texture-materials texture))
      (recompile :material material-name))
    (v:debug :zed "Recompiled texture: ~s" data)))
