(in-package #:cl-user)

(defpackage #:%zed.core.trait
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.core.context)
   (#:jobs #:%zed.game-object.jobs)
   (#:gob #:%zed.game-object))
  (:use #:cl))

(in-package #:%zed.core.trait)

(defstruct (trait
            (:constructor nil)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (context nil :type ctx::context)
  (owner nil :type (or gob::game-object null))
  (priority #.(1- (expt 2 32)) :type u:ub32)
  (setup-hook (constantly nil) :type function)
  (attach-hook (constantly nil) :type function)
  (detach-hook (constantly nil) :type function)
  (update-hook (constantly nil) :type function))

(u:define-printer (trait stream :type nil)
  (format stream "~s" (class-name (class-of trait))))

(defmacro define-trait (type (&key priority) &body (slots &optional options))
  (destructuring-bind (&key setup-hook attach-hook detach-hook update-hook) options
    `(defstruct (,type
                 (:include trait
                  (:priority ,(or priority #.(1- (expt 2 32))))
                  (:setup-hook ,(or setup-hook '(constantly nil)))
                  (:attach-hook ,(or attach-hook '(constantly nil)))
                  (:detach-hook ,(or detach-hook '(constantly nil)))
                  (:update-hook ,(or update-hook '(constantly nil))))
                 (:constructor ,(u:symbolicate '#:make- type) (context &key ,@(mapcar #'car slots)))
                 (:conc-name "")
                 (:predicate nil)
                 (:copier nil))
       ,@slots)))

;; Create an instance of a trait of the given type. Slow path, for when the type is not a quoted
;; symbol.
(u:fn-> make-trait (ctx::context symbol &rest t) trait)
(defun make-trait (context type &rest args)
  (declare (optimize speed))
  (if (subtypep type 'trait)
      (let ((trait (apply (fdefinition (u:symbolicate '#:make- type)) context args)))
        (funcall (setup-hook trait) trait)
        trait)
      (error "Trait type ~s is not defined." type)))

;; Create an instance of a trait of the given type. Fast path, for when the type is a quoted symbol.
(define-compiler-macro make-trait (&whole whole context type &rest args)
  (u:with-gensyms (trait)
    (if (and (consp type)
             (eq (car type) 'quote))
        (let* ((type (cadr type))
               (func (u:format-symbol (symbol-package type) "MAKE-~a" type)))
          (unless (subtypep type 'trait)
            (error "Trait type ~s is not defined." type))
          `(let ((,trait (,func ,context ,@args)))
             (funcall (setup-hook ,trait) ,trait)
             ,trait))
        whole)))

(u:fn-> attach (gob::game-object trait) null)
(defun attach (game-object trait)
  (declare (optimize speed))
  (when (owner trait)
    (error "Trait ~s is already attached to a game object." trait))
  (let ((jobs (ctx::jobs (context trait))))
    (setf (owner trait) game-object)
    (push (list game-object trait #'priority) (jobs::enable-traits jobs))
    (funcall (attach-hook trait) trait)
    nil))

(u:fn-> detach (gob::game-object trait) null)
(defun detach (game-object trait)
  (declare (optimize speed))
  (unless (eq game-object (owner trait))
    (error "Trait ~s is not attached to game object ~s." trait game-object))
  (let ((jobs (ctx::jobs (context trait))))
    (funcall (detach-hook trait) trait)
    (push (cons game-object trait) (jobs::disable-traits jobs))
    (setf (owner trait) nil)
    nil))

(u:fn-> detach-type (gob::game-object symbol) null)
(defun detach-type (game-object type)
  (declare (optimize speed))
  (dolist (x (gob::traits game-object))
    (when (eq (class-name (class-of x)) type)
      (detach game-object x))))

(u:fn-> detach-all (gob::game-object) null)
(defun detach-all (game-object)
  (declare (optimize speed))
  (dolist (x (gob::traits game-object))
    (detach game-object x)))
