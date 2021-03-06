(in-package #:zed)

(defmethod update-texture ((type (eql :2d-array)) texture source)
  (declare (optimize speed))
  (let* ((id (gl:gen-texture))
         (layer0 (first source))
         (width (image-width layer0))
         (height (image-height layer0)))
    (setf (texture-id texture) id
          (texture-width texture) width
          (texture-height texture) height)
    (gl:bind-texture :texture-2d-array id)
    (%gl:tex-storage-3d :texture-2d-array
                        (calculate-texture-mipmap-levels (texture-data texture) width height)
                        (image-internal-format layer0)
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
                                   (image-width image)
                                   (image-height image)
                                   1
                                   (image-pixel-format image)
                                   (image-pixel-type image)
                                   (image-data image)))
    (gl:bind-texture :texture-2d-array 0)))

(defmethod load-texture-source (data (type (eql :2d-array)) source &key width height)
  (declare (optimize speed))
  (cond
    ((null source)
     (list (load-framebuffer-texture data width height)))
    ((typep source '(integer 1 #.most-positive-fixnum))
     (loop :repeat (texture-data-source data)
           :collect (load-framebuffer-texture data width height)))
    ((and (typep source 'u:proper-list)
          (every #'listp source))
     (lp:pmapcar #'load-image source))
    (t (error "Unsupported source for 2D array texture: ~s." (texture-data-name data)))))
