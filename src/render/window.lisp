(in-package #:cl-user)

(defpackage #:%zed.render.window
  ;; Third-party packages
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:gl-context #:%zed.render.gl-context)
   (#:mon #:%zed.render.monitor))
  (:use #:cl))

(in-package #:%zed.render.window)

(defstruct (window
            (:constructor %make-window)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  ;; Foreign pointer to the underlying SDL2 window structure.
  (handle nil :type sdl2-ffi:sdl-window)
  ;; Reference to the monitor structure describing the physical monitor the window is displayed on.
  (monitor nil :type mon::monitor)
  ;; Foreign pointer to the OpenGL context exposed by SDL2.
  (gl-context nil :type sdl2-ffi::sdl-glcontext)
  ;; The current width in pixels of the window.
  (width 0 :type u:ub16)
  ;; The current height in pixels of the window.
  (height 0 :type u:ub16)
  ;; The current horizontal position in pixels of the window.
  (x 0 :type u:ub16)
  ;; The current vertical position in pixels of the window.
  (y 0 :type u:ub16)
  ;; The window title string, displayed in the window's titlebar.
  (title "" :type string))

(u:define-printer (window stream :type nil)
  (format stream "WINDOW: ~dx~d" (width window) (height window)))

(defun make-window (width height &key (title "") (anti-alias-p t))
  ;; First, we have to initialize SDL2's video subsystem.
  (sdl2:init* '(:video))
  ;; Before we create the window we have to prepare the OpenGL context so SDL2 knows how to prepare
  ;; the window appropriately.
  (gl-context::prepare-context :anti-alias-p anti-alias-p)
  ;; Create the window, along with the OpenGL context and monitor associations.
  (let ((window-handle (sdl2:create-window :w width :h height :title title :flags '(:opengl))))
    (%make-window :handle window-handle
                  :monitor (mon::make-monitor window-handle)
                  :gl-context (gl-context::make-context window-handle)
                  :width width
                  :height height
                  :title title)))

(defun move (window x y)
  (u:mvlet ((monitor-x monitor-y (mon::get-position (monitor window))))
    (setf (x window) (- x monitor-x)
          (y window) (- y monitor-y))))

(defun resize (window width height)
  (setf (width window) width
        (height window) height))

(defun draw (window)
  (sdl2:gl-swap-window (handle window)))

(defun destroy (window)
  (unwind-protect
       (progn
         (gl-context::destroy (gl-context window))
         (sdl2:destroy-window (handle window)))
    (sdl2:quit*)))