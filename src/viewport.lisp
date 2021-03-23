(in-package #:cl-user)

(defpackage #:%zed.viewport
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:vp.data #:%zed.viewport.data)
   (#:win #:%zed.window))
  (:use #:cl))

(in-package #:%zed.viewport)

(defstruct (viewport
            (:constructor %make-viewport)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data nil :type vp.data::data)
  (window nil :type win::window)
  picker ; TODO
  (x 0 :type u:ub16)
  (y 0 :type u:ub16)
  (width 0 :type u:ub16)
  (height 0 :type u:ub16))

(u:define-printer (viewport stream :type nil)
  (format stream "VIEWPORT: ~s" (vp.data::name (data viewport))))

(u:fn-> update (viewport) null)
(defun update (viewport)
  (declare (optimize speed))
  (let* ((data (data viewport))
         (window (window viewport))
         (window-width (win::width window))
         (window-height (win::height window)))
    (setf (x viewport) (truncate (u:lerp (vp.data::x data) 0 window-width))
          (y viewport) (truncate (u:lerp (vp.data::y data) 0 window-height))
          (width viewport) (truncate (u:lerp (vp.data::width data) 0 window-width))
          (height viewport) (truncate (u:lerp (vp.data::height data) 0 window-height)))
    nil))

(u:fn-> configure (viewport) null)
(declaim (inline configure))
(defun configure (viewport)
  (declare (optimize speed))
  (gl:viewport (x viewport) (y viewport) (width viewport) (height viewport))
  nil)

(u:fn-> make-viewport (symbol win::window) viewport)
(defun make-viewport (name window)
  (declare (optimize speed))
  (let ((viewport (%make-viewport :data (vp.data::find name) :window window)))
    (update viewport)
    viewport))
