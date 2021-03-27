(in-package #:cl-user)

(defpackage #:%zed.texture.cube-map-array
  ;; Third-party aliases
  (:local-nicknames
   (#:lp #:lparallel)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:img #:%zed.image)
   (#:tex #:%zed.texture))
  (:use #:cl))

(in-package #:%zed.texture.cube-map-array)

(defmethod tex::update ((type (eql :cube-map-array)) texture source)
  (declare (optimize speed))
  (let* ((id (gl:gen-texture))
         (source-length (list-length source))
         (first-layer (first source))
         (first-layer-face (first first-layer))
         (width (img::width first-layer-face))
         (height (img::height first-layer-face)))
    (declare (u:ub16 source-length))
    (setf (tex::id texture) id
          (tex::width texture) width
          (tex::height texture) height)
    (gl:bind-texture :texture-cube-map-array id)
    (%gl:tex-storage-3d :texture-cube-map-array
                        (tex::calculate-mipmap-levels (tex::data texture) width height)
                        (img::internal-format first-layer-face)
                        width
                        height
                        (* source-length 6))
    (loop :for layer :in source
          :for layer-index :of-type u:ub16 :from 0
          :do (loop :for image :in layer
                    :for face-index :of-type u:ub16 :from 0
                    :do (gl:tex-sub-image-3d :texture-cube-map-array
                                             0
                                             0
                                             0
                                             (+ (* layer-index 6) face-index)
                                             (img::width image)
                                             (img::height image)
                                             1
                                             (img::pixel-format image)
                                             (img::pixel-type image)
                                             (img::data image))))
    (gl:bind-texture :texture-cube-map-array 0)))

(defmethod tex::load-source (data (type (eql :cube-map-array)) source &key width height)
  (declare (optimize speed))
  (lp:pmapcar
   (lambda (x)
     (tex::load-source data :cube-map x :width width :height height))
   source))
