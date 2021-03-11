(in-package #:cl-user)

(defpackage #:%zed.core.trait
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.core.context)
   (#:jobs #:%zed.game-object.jobs)
   (#:gob #:%zed.game-object)
   (#:oc #:%zed.base.ordered-class))
  (:use #:cl))

(in-package #:%zed.core.trait)

(u:define-constant +slot-order+
    '(%context %owner %priority %setup-hook %attach-hook %detach-hook %update-hook)
  :test #'equal)

(u:eval-always
  (oc::define-ordered-class trait ()
    ((%context :reader context
               :initarg :context
               :inline t
               :type ctx::context)
     (%owner :accessor owner
             :inline t
             :type (or gob::game-object null)
             :initarg :owner
             :initform nil)
     (%priority :reader priority
                :inline t
                :type u:ub32
                :initarg :priority
                :initform #.(1- (expt 2 32)))
     (%setup-hook :reader setup-hook
                  :inline t
                  :type function
                  :initarg :setup-hook
                  :initform (constantly nil))
     (%attach-hook :reader attach-hook
                   :inline t
                   :type function
                   :initarg :attach-hook
                   :initform (constantly nil))
     (%detach-hook :reader detach-hook
                   :inline t
                   :type function
                   :initarg :detach-hook
                   :initform (constantly nil))
     (%update-hook :reader update-hook
                   :inline t
                   :type function
                   :initarg :update-hook
                   :initform (constantly nil)))
    (:order #.+slot-order+)))

(u:define-printer (trait stream :type nil)
  (format stream "~s" (class-name (class-of trait))))

(defmacro define-trait (type (&key priority) &body (slots . options))
  `(oc::define-ordered-class ,type (trait)
     ,slots
     (:order #.+slot-order+)
     ,@(when options
         `((:default-initargs
            ,@(when priority
                `(:priority ,priority))
            ,@(u:mappend
               (lambda (x)
                 (ecase (car x)
                   ((:setup-hook :attach-hook :detach-hook :update-hook)
                    x)))
               options))))))

;; Create an instance of a trait of the given type. Slow path, for when the type is not a quoted
;; symbol.
(u:fn-> make-trait (ctx::context symbol &rest t) trait)
(defun make-trait (context type &rest args)
  (declare (optimize speed))
  (if (subtypep type 'trait)
      (let ((trait (apply #'make-instance type :context context args)))
        (funcall (setup-hook trait) trait)
        trait)
      (error "Trait type ~s is not defined." type)))

;; Create an instance of a trait of the given type. Fast path, for when the type is a quoted symbol.
(define-compiler-macro make-trait (&whole whole context type &rest args)
  (u:with-gensyms (trait)
    (if (and (consp type)
             (eq (car type) 'quote))
        (let ((type (cadr type)))
          (unless (subtypep type 'trait)
            (error "Trait type ~s is not defined." type))
          `(let ((,trait (make-instance ',type :context ,context ,@args)))
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
