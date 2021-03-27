(in-package #:cl-user)

(defpackage #:%zed.texture.cube-map
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

(in-package #:%zed.texture.cube-map)

(defmethod tex::update ((type (eql :cube-map)) texture source)
  (declare (optimize speed))
  (let* ((id (gl:gen-texture))
         (layer0 (first source))
         (width (img::width layer0))
         (height (img::height layer0))
         (faces '(:texture-cube-map-positive-x
                  :texture-cube-map-negative-x
                  :texture-cube-map-positive-y
                  :texture-cube-map-negative-y
                  :texture-cube-map-positive-z
                  :texture-cube-map-negative-z)))
    (setf (tex::id texture) id
          (tex::width texture) width
          (tex::height texture) height)
    (gl:bind-texture :texture-cube-map id)
    (%gl:tex-storage-2d :texture-cube-map
                        (tex::calculate-mipmap-levels (tex::data texture) width height)
                        (img::internal-format layer0)
                        width
                        height)
    (loop :for image :in source
          :for face :in faces
          :do (gl:tex-sub-image-2d face
                                   0
                                   0
                                   0
                                   (img::width image)
                                   (img::height image)
                                   (img::pixel-format image)
                                   (img::pixel-type image)
                                   (img::data image)))
    (gl:bind-texture :texture-cube-map 0)))

(defmethod tex::load-source (context data (type (eql :cube-map)) source &key width height)
  (declare (optimize speed))
  (let ((valid-keys '(:x+ :x- :y+ :y- :z+ :z-)))
    (cond
      ((typep source '(or null (integer 1 1)))
       (loop :repeat 6
             :collect (tex::load-framebuffer-texture context data width height)))
      ((and (u:plist-p source)
            (u:set-equal (u:plist-keys source) valid-keys)
            (every #'listp (u:plist-values source)))
       (loop :for (k v) :on source :by #'cddr
             :collect k :into result
             :collect v :into result
             :finally (destructuring-bind (&key x+ x- y+ y- z+ z-) result
                        (return (lp:pmapcar
                                 (lambda (x) (img::load context x))
                                 (list x+ x- y+ y- z+ z-))))))
      (t (error "Unsupported source for cube map texture: ~s." (tex.data::name data))))))
