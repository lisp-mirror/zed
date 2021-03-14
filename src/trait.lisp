(in-package #:cl-user)

(defpackage #:%zed.trait
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:jobs #:%zed.jobs)
   (#:gob #:%zed.game-object)
   (#:oc #:%zed.ordered-class))
  (:use #:cl)
  (:shadow
   #:find))

(in-package #:%zed.trait)

(u:define-constant +slot-order+
    '(%context %owner %priority %setup-hook %attach-hook %detach-hook %update-hook %pre-render-hook
      %render-hook)
  :test #'equal)

(u:define-constant +hook-names+
    '(:setup-hook :attach-hook :detach-hook :update-hook :pre-render-hook :render-hook)
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
                  :type symbol
                  :initarg :setup-hook
                  :initform 'default-hook)
     (%attach-hook :reader attach-hook
                   :inline t
                   :type symbol
                   :initarg :attach-hook
                   :initform 'default-hook)
     (%detach-hook :reader detach-hook
                   :inline t
                   :type symbol
                   :initarg :detach-hook
                   :initform 'default-hook)
     (%update-hook :reader update-hook
                   :inline t
                   :type symbol
                   :initarg :update-hook
                   :initform 'default-hook)
     (%pre-render-hook :reader pre-render-hook
                       :inline t
                       :type symbol
                       :initarg :pre-render-hook
                       :initform 'default-hook)
     (%render-hook :reader render-hook
                   :inline t
                   :type symbol
                   :initarg :render-hook
                   :initform 'default-hook))
    (:order #.+slot-order+)))

(u:fn-> default-hook (trait) null)
(defun default-hook (trait)
  (declare (optimize speed))
  (declare (ignore trait))
  nil)

(u:fn-> get-type (trait) symbol)
(declaim (inline get-type))
(defun get-type (trait)
  (declare (optimize speed))
  (values (class-name (class-of trait))))

(u:define-printer (trait stream :type nil)
  (format stream "TRAIT: ~s" (get-type trait)))

(defun generate-initargs (type priority options)
  (when (or priority options)
    `((:default-initargs
       ,@(when priority
           `(:priority ,priority))
       ,@(u:mappend
          (lambda (x)
            (destructuring-bind (key value) x
              (ecase key
                (#.+hook-names+
                 (if (symbolp value)
                     `(,key ',value)
                     (error "~s for trait ~s should be an unquoted symbol but got: ~s"
                            key
                            type
                            value))))))
          options)))))

(defmacro define-internal-trait (type (&key priority) &body (slots . options))
  `(oc::define-ordered-class ,type (trait)
     ,slots
     (:order (,@+slot-order+ ,@(mapcar #'car slots)))
     ,@(generate-initargs type priority options)))

(defmacro define-trait (type (&key priority) &body (slots . options))
  `(oc::define-ordered-class ,type (trait)
     ,slots
     (:order ,+slot-order+)
     ,@(generate-initargs type priority options)))

;; Create an instance of a trait of the given type. Slow path, for when the type is not a quoted
;; symbol.
(u:fn-> make-trait (ctx::context symbol &rest t) trait)
(defun make-trait (context type &rest args)
  (declare (optimize speed))
  (if (subtypep type 'trait)
      (let ((trait (apply #'make-instance type :context context args)))
        (funcall (fdefinition (setup-hook trait)) trait)
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
             (funcall (fdefinition (setup-hook ,trait)) ,trait)
             ,trait))
        whole)))

(u:fn-> find (gob::game-object symbol) trait)
(defun find (game-object type)
  (declare (optimize speed))
  (cl:find type (gob::traits game-object) :key #'get-type))

(u:fn-> attach (gob::game-object trait) null)
(defun attach (game-object trait)
  (declare (optimize speed))
  (when (owner trait)
    (error "Trait ~s is already attached to a game object." trait))
  (let ((jobs (ctx::jobs (context trait))))
    (setf (owner trait) game-object)
    (push (list game-object trait #'priority) (jobs::enable-traits jobs))
    (funcall (fdefinition (attach-hook trait)) trait)
    nil))

(u:fn-> detach (gob::game-object trait) null)
(defun detach (game-object trait)
  (declare (optimize speed))
  (unless (eq game-object (owner trait))
    (error "Trait ~s is not attached to game object ~s." trait game-object))
  (let ((jobs (ctx::jobs (context trait))))
    (funcall (fdefinition (detach-hook trait)) trait)
    (push (cons game-object trait) (jobs::disable-traits jobs))
    (setf (owner trait) nil)
    nil))

(u:fn-> detach-type (gob::game-object symbol) null)
(defun detach-type (game-object type)
  (declare (optimize speed))
  (dolist (x (gob::traits game-object))
    (when (eq (get-type x) type)
      (detach game-object x))))

(u:fn-> detach-all (gob::game-object) null)
(defun detach-all (game-object)
  (declare (optimize speed))
  (dolist (x (gob::traits game-object))
    (detach game-object x)))
