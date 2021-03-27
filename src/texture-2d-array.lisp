(in-package #:cl-user)

(defpackage #:%zed.texture.2d-array
  ;; Third-party aliases
  (:local-nicknames
   (#:lp #:lparallel)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:img #:%zed.image)
   (#:tex #:%zed.texture)
   (#:tex.data #:%zed.texture.data))
  (:use #:cl))

(in-package #:%zed.texture.2d-array)

(defmethod tex::update ((type (eql :2d-array)) texture source)
  (declare (optimize speed))
  (let* ((id (gl:gen-texture))
         (layer0 (first source))
         (width (img::width layer0))
         (height (img::height layer0)))
    (setf (tex::id texture) id
          (tex::width texture) width
          (tex::height texture) height)
    (gl:bind-texture :texture-2d-array id)
    (%gl:tex-storage-3d :texture-2d-array
                        (tex::calculate-mipmap-levels (tex::data texture) width height)
                        (img::internal-format layer0)
                        width
                        height
                        (length source))
    (loop :for image :in source
          :for layer :of-type fixnum :from 0
          :do (gl:tex-sub-image-3d :texture-2d-array
                                   0
                                   0
                                   0
                                   layer
                                   (img::width image)
                                   (img::height image)
                                   1
                                   (img::pixel-format image)
                                   (img::pixel-type image)
                                   (img::data image)))
    (gl:bind-texture :texture-2d-array 0)))

(defmethod tex::load-source (data (type (eql :2d-array)) source &key width height)
  (declare (optimize speed))
  (cond
    ((null source)
     (list (tex::load-framebuffer-texture data width height)))
    ((typep source '(integer 1 #.most-positive-fixnum))
     (loop :repeat (tex.data::source data)
           :collect (tex::load-framebuffer-texture data width height)))
    ((and (typep source 'u:proper-list)
          (every #'listp source))
     (lp:pmapcar #'img::load source))
    (t (error "Unsupported source for 2D array texture: ~s." (tex.data::name data)))))
