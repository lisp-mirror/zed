(in-package #:cl-user)

;;;; The context is the top-level object containing all state necessary for communication between
;;;; modules during the lifetime of the runtime. It is constructed early  directly before initiating
;;;; the main game loop.

(defpackage #:%zed.core.context
  (:local-nicknames
   (#:cfg #:%zed.core.config)
   (#:clock #:%zed.core.clock)
   (#:in #:%zed.input)
   (#:in.man #:%zed.input.manager)
   (#:live #:%zed.core.live-coding)
   (#:mon #:%zed.render-backend.monitor)
   (#:win #:%zed.render-backend.window))
  (:use #:cl))

(in-package #:%zed.core.context)

(defstruct (context
            (:constructor %make-context)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (running-p nil :type boolean)
  (clock nil :type clock::clock)
  (window nil :type win::window)
  (input-manager nil :type in.man::manager))

;;; The current context is bound to this variable throughout the lifetime of the game. However, this
;;; should not be used in code. This only exists for internal debugging purposes. It is a core
;;; design decision not to have any global state, as it is the source of many bugs and hard to
;;; reason about code. The reason this exists is two-fold: To quickly troubleshoot issues by
;;; inspecting the currently bound value in the REPL as the engine is executing, and it may be used
;;; as a hoist for compile-time constructs such as declarative DSLs that wish to read or write into
;;; the running game state.
(defvar *context* nil)

;; Construct the context with everything needed to enter the main game loop.
(defun make-context (config)
  (let* (;; Create a window for drawing.
         (window (win::make-window (cfg::window-width config)
                                   (cfg::window-height config)
                                   :title (cfg::window-title config)
                                   :anti-alias-p (cfg::anti-alias-p config)))
         ;; Initialize the clock using either the user-supplied delta-time or defaulting to the
         ;; inverse monitor refresh rate.
         (clock (clock::make-clock (or (cfg::delta-time config)
                                       (/ (mon::get-refresh-rate (win::monitor window))))))
         ;; Initialize the input manager.
         (input-manager (in::make-input-manager)))
    ;; Setup live coding support. This instructs SLIME or Sly's REPL to run inside our game loop.
    (live::setup-repl)
    ;; Construct the context with references to the previously constructed state.
    (%make-context :running-p t
                   :clock clock
                   :window window
                   :input-manager input-manager)))

(defun destroy (context)
  ;; Destroy the window, which takes care of cleaning up any foreign resources for the window and
  ;; associated OpenGL context.
  (win::destroy (window context))
  ;; Force the Lisp implementation to perform a full garbage collection.
  (tg:gc :full t))
