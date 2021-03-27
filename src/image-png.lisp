(in-package #:cl-user)

(defpackage #:%zed.image.png
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:img #:%zed.image))
  (:use #:cl))

(in-package #:%zed.image.png)

(defun get-channel-count (image)
  (ecase (pngload:color-type image)
    (:greyscale 1)
    (:greyscale-alpha 2)
    (:truecolour 3)
    (:truecolour-alpha 4)))

(defun get-pixel-format (image)
  (ecase (pngload:color-type image)
    (:greyscale :red)
    (:greyscale-alpha :rg)
    (:truecolour :rgb)
    (:truecolour-alpha :rgba)))

(defun get-pixel-type (image)
  (ecase (pngload:bit-depth image)
    (8 :unsigned-byte)
    (16 :unsigned-short)))

(defun get-internal-format (image)
  (let ((channel-count (get-channel-count image))
        (bit-depth (pngload:bit-depth image)))
    (u:format-symbol :keyword "~a~d" (subseq "RGBA" 0 channel-count) bit-depth)))

(defmethod img::%load ((type (eql :png)) stream)
  (let ((image (pngload:load-stream stream :flatten t :flip-y t)))
    (img::make-image :width (pngload:width image)
                     :height (pngload:height image)
                     :pixel-format (get-pixel-format image)
                     :pixel-type (get-pixel-type image)
                     :internal-format (get-internal-format image)
                     :data (pngload:data image))))
