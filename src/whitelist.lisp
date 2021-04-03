(in-package #:zed)

(defvar *current-whitelist-scope* nil)

(u:eval-always
  (defun whitelist-scope-name->flag-name (scope)
    (let ((symbol (u:format-symbol :zed "+~a+" scope)))
      symbol))

  (defun whitelist-scope-name->flag (scope)
    (let ((flag-name (whitelist-scope-name->flag-name scope)))
      (if (boundp flag-name)
          (symbol-value flag-name)
          (error "Invalid scope name: ~a" scope)))))

(defmacro define-whitelist-scopes (() &body body)
  `(progn
     ,@(loop :for scope :in body
             :for name = (whitelist-scope-name->flag-name scope)
             :for i :from 0
             :for value = (ash 1 i)
             :collect `(u:define-constant ,name ,value))
     (defun whitelist-scope-value->name (value)
       (ecase value
         ,@(loop :for scope :in body
                 :for i :from 0
                 :for value = (ash 1 i)
                 :collect `(,value ,scope))))))

(defmacro with-scope ((scope) &body body)
  `(let ((*current-whitelist-scope* ,(whitelist-scope-name->flag scope)))
     ,@body))

(defmacro with-allowed-scopes (function-name scope-names &body body)
  (let ((scopes-mask (apply #'logior (mapcar #'whitelist-scope-name->flag scope-names))))
    (if (member :zed.release *features*)
        `(progn ,@body)
        `(progn
           (when (zerop (logand *current-whitelist-scope* ,scopes-mask))
             (error "Whitelisted feature ~a can only be used in certain scopes:~%~%~
                     Allowed scopes: ~{~a~^, ~}~%Illegal calling scope: ~a"
                    ',function-name
                    ',scope-names
                    (whitelist-scope-value->name *current-whitelist-scope*)))
           ,@body))))

(define-whitelist-scopes ()
  :prelude
  :prefab-instantiate
  :prefab-recompile
  :physics-phase
  :update-phase
  :trait-setup-hook
  :trait-destroy-hook
  :trait-attach-hook
  :trait-detach-hook
  :trait-physics-hook
  :trait-update-hook
  :trait-render-hook)
