(in-package #:zed)

(defstruct (window
            (:constructor %make-window)
            (:predicate nil)
            (:copier nil))
  ;; Foreign pointer to the underlying SDL2 window structure.
  (handle nil :type sdl2-ffi:sdl-window)
  ;; Reference to the monitor structure describing the physical monitor the window is displayed on.
  (monitor nil :type monitor)
  ;; Foreign pointer to the OpenGL context exposed by SDL2.
  (gl-context nil :type sdl2-ffi::sdl-glcontext)
  ;; The current width in pixels of the window.
  (width 0 :type u:ub16)
  ;; The current height in pixels of the window.
  (height 0 :type u:ub16)
  ;; The current horizontal position in pixels of the window.
  (x 0 :type u:b32)
  ;; The current vertical position in pixels of the window.
  (y 0 :type u:b32)
  ;; The window title string, displayed in the window's titlebar.
  (title "" :type string))

(u:define-printer (window stream :type nil)
  (format stream "WINDOW: ~dx~d" (window-width window) (window-height window)))

(u:fn-> move-window (window u:b32 u:b32) null)
(defun move-window (window x y)
  (declare (optimize speed))
  (u:mvlet ((monitor-x monitor-y (get-monitor-position (window-monitor window))))
    (declare (u:ub16 monitor-x monitor-y))
    (setf (window-x window) (- x monitor-x)
          (window-y window) (- y monitor-y))
    nil))

(u:fn-> resize-window (window u:ub16 u:ub16) null)
(declaim (inline resize-window))
(defun resize-window (window width height)
  (declare (optimize speed))
  (setf (window-width window) width
        (window-height window) height)
  nil)

(u:fn-> draw-window (window) null)
(declaim (inline draw-window))
(defun draw-window (window)
  (declare (optimize speed))
  (sdl2:gl-swap-window (window-handle window))
  nil)

(u:fn-> destroy-window (window) null)
(defun destroy-window (window)
  (declare (optimize speed))
  (unwind-protect
       (progn
         (destroy-opengl-context (window-gl-context window))
         (sdl2:destroy-window (window-handle window)))
    (sdl2:quit*))
  nil)

(defun make-window (width height &key (title "") (anti-alias-p t) (vsync-p t))
  ;; First, we have to initialize SDL2's video subsystem.
  (sdl2:init* '(:video))
  ;; Before we create the window we have to prepare the OpenGL context so SDL2 knows how to prepare
  ;; the window appropriately.
  (prepare-opengl-context :anti-alias-p anti-alias-p)
  ;; Create the window, along with the OpenGL context and monitor associations.
  (let* ((window-handle (sdl2:create-window :w width :h height :title title :flags '(:opengl)))
         (window (%make-window :handle window-handle
                               :monitor (make-monitor window-handle)
                               :gl-context (make-opengl-context window-handle vsync-p)
                               :width width
                               :height height
                               :title title)))
    (draw-window window)
    (v:info :zed "Created window (~dx~d)" width height)
    window))
