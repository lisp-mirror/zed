(in-package #:cl-user)

(defpackage #:%zed.render.monitor
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.render.monitor)

(defstruct (monitor
            (:constructor %make-monitor)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (id 0 :type u:ub8)
  (width 0 :type u:ub16)
  (height 0 :type u:ub16)
  (x 0 :type u:ub16)
  (y 0 :type u:ub16)
  (refresh-rate 0 :type u:ub8))

(u:define-printer (monitor stream :type nil)
  (format stream "MONITOR: ~dx~d @ ~dHz"
          (width monitor)
          (height monitor)
          (refresh-rate monitor)))

(defun get-id (window-handle)
  (sdl2-ffi.functions:sdl-get-window-display-index window-handle))

(defgeneric get-dimensions (monitor)
  (:method ((monitor monitor))
    (get-dimensions (id monitor)))
  (:method ((monitor integer))
    (u:mvlet ((format width height refresh-rate (sdl2:get-current-display-mode monitor)))
      (values width height))))

(defgeneric get-position (monitor)
  (:method ((monitor monitor))
    (get-position (id monitor)))
  (:method ((monitor integer))
    (let* ((rect (sdl2:get-display-bounds monitor))
           (x (sdl2:rect-x rect))
           (y (sdl2:rect-y rect)))
      (sdl2:free-rect rect)
      (values x y))))

(defgeneric get-refresh-rate (monitor)
  (:method ((monitor monitor))
    (get-refresh-rate (id monitor)))
  (:method ((monitor integer))
    (u:mvlet ((format width height refresh-rate (sdl2:get-current-display-mode monitor)))
      refresh-rate)))

(defun make-monitor (window-handle)
  (u:mvlet* ((id (get-id window-handle))
             (width height (get-dimensions id))
             (x y (get-position id)))
    (%make-monitor :id id
                   :width width
                   :height height
                   :x x
                   :y y
                   :refresh-rate (get-refresh-rate id))))
