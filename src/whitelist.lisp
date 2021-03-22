(in-package #:cl-user)

(defpackage #:%zed.whitelist
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames)
  (:use #:cl))

(in-package #:%zed.whitelist)

(defvar *current-hook* nil)

(defmacro with-allowed-hooks (function-name hooks &body body)
  (if (member :zed.release *features*)
      `(progn ,@body)
      `(progn
         (unless (find *current-hook* ',hooks)
           (error "Restricted function ~a can only be called from certain hooks:~%~%~
                   Allowed hooks: ~{~a~^, ~}~%Called from hook: ~a"
                  ',function-name
                  ',hooks
                  *current-hook*))
         ,@body)))
