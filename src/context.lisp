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
   (#:aud #:%zed.audio)
   (#:cfg #:%zed.config)
   (#:clock #:%zed.clock)
   (#:gob #:%zed.game-object)
   (#:in #:%zed.input)
   (#:in.mgr #:%zed.input.manager)
   (#:jobs #:%zed.jobs)
   (#:live #:%zed.live-coding)
   (#:log #:%zed.logging)
   (#:mon #:%zed.monitor)
   (#:pack #:%zed.pack)
   (#:shd.mgr #:%zed.shader.manager)
   (#:tp #:%zed.thread-pool)
   (#:util #:%zed.util)
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
  (window nil :type win::window)
  (clock nil :type clock::clock)
  (input-manager nil :type in.mgr::manager)
  (shader-manager (shd.mgr::make-manager) :type shd.mgr::manager)
  (scene-tree (gob::make-root) :type gob::game-object)
  (jobs (jobs::make-jobs) :type jobs::jobs)
  (resource-cache (u:dict #'eq) :type hash-table)
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
  (let ((success-p nil))
    (unwind-protect
         (progn
           ;; Enable logging.
           (log::start config)
           (let* ((window (win::make-window (cfg::window-width config)
                                            (cfg::window-height config)
                                            :title (cfg::window-title config)
                                            :anti-alias-p (cfg::anti-alias-p config)))
                  (refresh-rate (mon::get-refresh-rate (win::monitor window))))
             ;; Start the audio system.
             (aud::start)
             ;; Setup live coding support. This instructs SLIME or Sly's REPL to run inside our game
             ;; loop.
             (live::setup-repl)
             ;; Load the pack file (if running in release mode).
             (pack::read-pack)
             ;; Register all defined shader programs with the thread pool so they are updated when
             ;; recompiled at runtime.
             (shd.mgr::register-shaders)
             ;; Construct the context with references to the previously constructed state.
             (prog1 (%make-context :running-p t
                                   :clock (clock::make-clock config refresh-rate)
                                   :window window
                                   :input-manager (in::make-input-manager)
                                   :viewports (vp.mgr::make-manager window))
               (setf success-p t))))
      (unless success-p
        (sdl2:quit*)))))

;; This is called when the main game loop exits to destroy the context. All code should call
;; #'shutdown instead, which initiates a graceful shutdown of the context.
(defun destroy (context)
  (v:info :zed.context "Destroying context...")
  ;; Destroy the input manager.
  (in::destroy (input-manager context))
  ;; Destroy the window, which takes care of cleaning up any foreign resources for the window and
  ;; associated OpenGL context.
  (win::destroy (window context))
  ;; Stop the audio system
  (aud::stop)
  ;; Stop logging.
  (log::stop)
  ;; Force the Lisp implementation to perform a full garbage collection.
  (tg:gc :full t)
  (v:info :zed.context "Context destroyed"))

;; Gracefully shut down the context. This instructs the main game loop to exit at the right time
;; (not mid-iteration), and initiates the graceful destruction of the context.
(defun shutdown (context)
  (setf (running-p context) nil))

(defmacro with-context (context (config) &body body)
  `(if util::=context=
       (warn "There is a context already running.")
       (let* ((,context (make-context ,config)))
         (setf util::=context= ,context)
         (tp::with-thread-pool ()
           (unwind-protect
                (progn
                  (wl::with-scope (:prelude)
                    (v:debug :zed.context "Executing prelude...")
                    (funcall (cfg::prelude ,config) ,context)
                    (v:debug :zed.context "Finished executing prelude"))
                  ,@body)
             (setf util::=context= nil)
             (destroy ,context))))))
