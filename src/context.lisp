(in-package #:zed)

;; Construct the context with everything needed to enter the main game loop.
(defun make-context (config)
  (let ((success-p nil))
    (unwind-protect
         (progn
           ;; Enable logging.
           (start-logging config)
           (let* ((window (make-window (config-window-width config)
                                       (config-window-height config)
                                       :title (config-window-title config)
                                       :anti-alias-p (config-anti-alias-p config)))
                  (refresh-rate (get-monitor-refresh-rate (window-monitor window)))
                  (collision-plan (or (config-collision-plan config) :default)))
             ;; Start the audio system.
             (start-audio)
             ;; Setup live coding support. This instructs SLIME or Sly's REPL to run inside our game
             ;; loop.
             (setup-repl)
             ;; Load the pack file (if running in release mode).
             (read-pack)
             ;; Register all defined shader programs with the thread pool so they are updated when
             ;; recompiled at runtime.
             (register-shaders)
             ;; Construct the context with references to the previously constructed state.
             (prog1 (%make-context :running-p t
                                   :clock (make-clock config refresh-rate)
                                   :window window
                                   :input-manager (make-input-manager)
                                   :viewports (make-viewport-manager window)
                                   :collision-system (make-collision-system collision-plan))
               (setf success-p t))))
      (unless success-p
        (sdl2:quit*)))))

;; This is called when the main game loop exits to destroy the context. All code should call
;; #'shutdown instead, which initiates a graceful shutdown of the context.
(defun destroy-context (context)
  (v:info :zed "Destroying context...")
  ;; Destroy the input manager.
  (destroy-input-manager (context-input-manager context))
  ;; Destroy the window, which takes care of cleaning up any foreign resources for the window and
  ;; associated OpenGL context.
  (destroy-window (context-window context))
  ;; Stop the audio system
  (stop-audio)
  ;; Stop logging.
  (stop-logging)
  ;; Force the Lisp implementation to perform a full garbage collection.
  (tg:gc :full t)
  (v:info :zed "Context destroyed"))

;; Gracefully shut down the context. This instructs the main game loop to exit at the right time
;; (not mid-iteration), and initiates the graceful destruction of the context.
(defun shutdown-context (context)
  (setf (context-running-p context) nil))

(defmacro with-context (context (config) &body body)
  `(if =context=
       (warn "There is a context already running.")
       (let* ((,context (make-context ,config)))
         (setf =context= ,context)
         (with-thread-pool ()
           (unwind-protect
                (progn
                  (with-scope (:prelude)
                    (v:debug :zed "Executing prelude...")
                    (funcall (config-prelude ,config) ,context)
                    (v:debug :zed "Finished executing prelude"))
                  ,@body)
             (setf =context= nil)
             (destroy-context ,context))))))
