(in-package #:cl-user)

;;;; The context is the top-level object containing all state necessary for communication between
;;;; modules during the lifetime of the runtime. It is constructed early  directly before initiating
;;;; the main game loop.

(defpackage #:%zed.context
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:cfg #:%zed.config)
   (#:clock #:%zed.clock)
   (#:dbg #:%zed.debug)
   (#:gob #:%zed.game-object)
   (#:in #:%zed.input)
   (#:in.mgr #:%zed.input.manager)
   (#:jobs #:%zed.jobs)
   (#:live #:%zed.live-coding)
   (#:mon #:%zed.monitor)
   (#:sbs #:%zed.shader-buffer-state)
   (#:shd #:%zed.shader-program)
   (#:tp #:%zed.thread-pool)
   (#:vp.mgr #:%zed.viewport.manager)
   (#:win #:%zed.window)
   (#:wl #:%zed.whitelist))
  (:use #:cl))

(in-package #:%zed.context)

(defstruct (context
            (:constructor %make-context)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (running-p nil :type boolean)
  (thread-pool nil :type tp::thread-pool)
  (window nil :type win::window)
  (clock nil :type clock::clock)
  (input-manager nil :type in.mgr::manager)
  (shader-buffer-state (sbs::make-state) :type sbs::state)
  (scene-tree (gob::make-root) :type gob::game-object)
  (jobs (jobs::make-jobs) :type jobs::jobs)
  (assets (u:dict #'eq) :type hash-table)
  (framebuffers (u:dict #'eq) :type hash-table)
  (materials (u:dict #'eq) :type hash-table)
  (prefabs (u:dict #'eq) :type hash-table)
  (viewports nil :type vp.mgr::manager)
  (draw-order nil)
  (active-camera nil))

(u:define-printer (context stream :type nil :identity t)
  (format stream "CONTEXT"))

;; Construct the context with everything needed to enter the main game loop.
(defun make-context (config)
  (let* (;; Create the thread pool.
         (thread-pool (tp::make-thread-pool))
         ;; Create a window for drawing.
         (window (win::make-window (cfg::window-width config)
                                   (cfg::window-height config)
                                   :title (cfg::window-title config)
                                   :anti-alias-p (cfg::anti-alias-p config)))
         ;; Initialize the clock using either the user-supplied delta-time or defaulting to the
         ;; inverse monitor refresh rate.
         (clock (clock::make-clock config (mon::get-refresh-rate (win::monitor window))))
         (input-manager (in::make-input-manager))
         (viewport-manager (vp.mgr::make-manager window)))
    ;; Setup live coding support. This instructs SLIME or Sly's REPL to run inside our game loop.
    (live::setup-repl)
    ;; Register all defined shader programs with the thread pool so they are updated when recompiled
    ;; at runtime.
    (shd::register-shaders thread-pool)
    ;; Construct the context with references to the previously constructed state.
    (%make-context :running-p t
                   :thread-pool thread-pool
                   :clock clock
                   :window window
                   :input-manager input-manager
                   :viewports viewport-manager)))

;; This is called when the main game loop exits to destroy the context. All code should call
;; #'shutdown instead, which initiates a graceful shutdown of the context.
(defun destroy (context)
  ;; Destroy the input manager.
  (in::destroy (input-manager context))
  ;; Destroy the window, which takes care of cleaning up any foreign resources for the window and
  ;; associated OpenGL context.
  (win::destroy (window context))
  ;; Force the Lisp implementation to perform a full garbage collection.
  (tg:gc :full t))

;; Gracefully shut down the context. This instructs the main game loop to exit at the right time
;; (not mid-iteration), and initiates the graceful destruction of the context.
(defun shutdown (context)
  (setf (running-p context) nil))

(defmacro with-context (context (config) &body body)
  `(if dbg::=context=
       (warn "There is a context already running.")
       (let* ((,context (make-context ,config)))
         (setf dbg::=context= ,context)
         (tp::with-thread-pool (thread-pool ,context)
           (unwind-protect
                (progn
                  (wl::with-scope (:prelude)
                    (funcall (cfg::prelude ,config) ,context))
                  ,@body)
             (setf dbg::=context= nil)
             (destroy ,context))))))
