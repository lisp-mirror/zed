(in-package #:cl-user)

(defpackage #:%zed.util
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.util)

(glob:define-global-var =system-name= nil)

;;; The current context is bound to this variable throughout the lifetime of the game. However, this
;;; should not be used in code. This only exists for internal debugging purposes. It is a core
;;; design decision not to have any global state, as it is the source of many bugs and hard to
;;; reason about code. The reason this exists is two-fold: To quickly troubleshoot issues by
;;; inspecting the currently bound value in the REPL as the engine is executing, and it may be used
;;; as a hoist for compile-time constructs such as declarative DSLs that wish to read or write into
;;; the running game state.
(glob:define-global-var =context= nil)

(defmacro check (&body body)
  (unless (member :zed.release *features*)
    `(progn
       ,@(mapcar
          (lambda (x)
            `(unless ,x
               (error "Debug check failed: ~s" ',x)))
          body))))

(defmacro with-debug-group (name &body body)
  (if (member :zed.release *features*)
      `(progn ,@body)
      (u:once-only (name)
        `(progn
           (cffi:with-foreign-string (s ,name)
             (%gl:push-debug-group :debug-source-application 0 (length ,name) s))
           (unwind-protect (progn ,@body)
             (%gl:pop-debug-group))))))

(defmacro with-profiling ((enable-p) &body body)
  (let ((packages (remove-if-not
                   (lambda (x)
                     (or (u:string-starts-with-p x "%ZED")
                         (u:string-starts-with-p x "ZED")
                         (u:string-starts-with-p x "ORIGIN")
                         (u:string-starts-with-p x "CL-OPENGL")
                         (string= x "SHADOW")
                         (string= x "SDL2")))
                   (mapcar #'package-name (list-all-packages)))))
    `(if ,enable-p
         (unwind-protect
              (progn
                (sb-profile:unprofile)
                (sb-profile:profile ,@packages)
                ,@body)
           (sb-profile:report)
           (sb-profile:unprofile)
           (sb-profile:reset))
         (progn ,@body))))
