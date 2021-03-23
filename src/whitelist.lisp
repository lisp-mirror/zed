(in-package #:cl-user)

(defpackage #:%zed.whitelist
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames)
  (:use #:cl))

(in-package #:%zed.whitelist)

(defvar *current-scope* nil)

(u:eval-always
  (defun scope-name->flag-name (scope)
    (let ((symbol (u:format-symbol :%zed.whitelist "+~a+" scope)))
      symbol))

  (defun scope-name->flag (scope)
    (let ((flag-name (scope-name->flag-name scope)))
      (if (boundp flag-name)
          (symbol-value flag-name)
          (error "Invalid scope name: ~a" scope)))))

(defmacro define-scopes (() &body body)
  `(progn
     ,@(loop :for scope :in body
             :for name = (scope-name->flag-name scope)
             :for i :from 0
             :for value = (ash 1 i)
             :collect `(u:define-constant ,name ,value))
     (defun scope-value->name (value)
       (ecase value
         ,@(loop :for scope :in body
                 :for i :from 0
                 :for value = (ash 1 i)
                 :collect `(,value ,scope))))))

(defmacro with-scope ((scope) &body body)
  `(let ((*current-scope* ,(scope-name->flag scope)))
     ,@body))

(defmacro with-allowed-scopes (function-name scope-names &body body)
  (let ((scopes-mask (apply #'logior (mapcar #'scope-name->flag scope-names))))
    (if (member :zed.release *features*)
        `(progn ,@body)
        `(progn
           (when (zerop (logand *current-scope* ,scopes-mask))
             (error "Whitelisted feature ~a can only be used in certain scopes:~%~%~
                     Allowed scopes: ~{~a~^, ~}~%Illegal calling scope: ~a"
                    ',function-name
                    ',scope-names
                    (scope-value->name *current-scope*)))
           ,@body))))

(define-scopes ()
  :prelude
  :prefab-instantiate
  :trait-setup-hook
  :trait-destroy-hook
  :trait-attach-hook
  :trait-detach-hook
  :trait-update-hook
  :trait-pre-render-hook
  :trait-render-hook)
