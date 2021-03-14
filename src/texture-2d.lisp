(in-package #:cl-user)

(defpackage #:%zed.texture.2d
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:img #:%zed.image)
   (#:tex #:%zed.texture)
   (#:tex.data #:%zed.texture.data))
  (:use #:cl))

(in-package #:%zed.texture.2d)

(defmethod tex::update ((type (eql :2d)) texture source)
  (declare (optimize speed))
  (let* ((id (gl:gen-texture))
         (width (img::width source))
         (height (img::height source)))
    (setf (tex::id texture) id
          (tex::width texture) width
          (tex::height texture) height)
    (gl:bind-texture :texture-2d id)
    (%gl:tex-storage-2d :texture-2d
                        (tex::calculate-mipmap-levels (tex::data texture) width height)
                        (img::internal-format source)
                        width
                        height)
    (u:when-let ((data (img::data source)))
      (gl:tex-sub-image-2d :texture-2d
                           0
                           0
                           0
                           width
                           height
                           (img::pixel-format source)
                           (img::pixel-type source)
                           data))
    (gl:bind-texture :texture-2d 0)))

(defmethod tex::load-source (data (type (eql :2d)) source &key width height)
  (declare (optimize speed))
  (typecase source
    ((or null (integer 1 1))
     (tex::load-framebuffer-texture data width height))
    (list
     (img::load (tex.data::source data)))
    (t (error "Unsupported source for 2D texture: ~s." (tex.data::name data)))))
