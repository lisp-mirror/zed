(in-package #:zed)

(u:define-constant +trait-slot-order+
    '(%%context %%owner %%priority %%setup-hook %%attach-hook %%detach-hook %%update-hook
      %%render-hook)
  :test #'equal)

(u:define-constant +trait-hook-names+
    '(setup destroy attach detach physics update render) :test #'equal)

(u:eval-always
  (util.oc::define-ordered-class trait ()
    ((%%context :reader trait-context
                :initarg context
                :inline t
                :type context)
     (%%owner :accessor trait-owner
              :inline t
              :type (or game-object null)
              :initform nil)
     (%%priority :reader trait-priority
                 :inline t
                 :type u:ub32
                 :initarg priority
                 :initform #.(1- (expt 2 32)))
     (%%setup-hook :reader trait-setup-hook
                   :inline t
                   :type symbol
                   :initarg setup
                   :initform 'default-trait-hook)
     (%%destroy-hook :reader trait-destroy-hook
                     :inline t
                     :type symbol
                     :initarg destroy
                     :initform 'default-trait-hook)
     (%%attach-hook :reader trait-attach-hook
                    :inline t
                    :type symbol
                    :initarg attach
                    :initform 'default-trait-hook)
     (%%detach-hook :reader trait-detach-hook
                    :inline t
                    :type symbol
                    :initarg detach
                    :initform 'default-trait-hook)
     (%%physics-hook :reader trait-physics-hook
                     :inline t
                     :type symbol
                     :initarg physics
                     :initform 'default-trait-hook)
     (%%update-hook :reader trait-update-hook
                    :inline t
                    :type symbol
                    :initarg update
                    :initform 'default-trait-hook)
     (%%render-hook :reader trait-render-hook
                    :inline t
                    :type symbol
                    :initarg render
                    :initform 'default-trait-hook))
    (:order #.+trait-slot-order+)))

(u:fn-> get-trait-type (trait) symbol)
(declaim (inline get-trait-type))
(defun get-trait-type (trait)
  (declare (optimize speed))
  (values (class-name (class-of trait))))

(u:define-printer (trait stream :type nil)
  (format stream "TRAIT: ~s" (get-trait-type trait)))

(u:fn-> default-trait-hook (trait) null)
(defun default-trait-hook (trait)
  (declare (optimize speed)
           (ignore trait))
  nil)

(defun generate-trait-initargs (type priority options)
  (when (or priority options)
    `((:default-initargs
       ,@(when priority
           `(priority ,priority))
       ,@(u:mappend
          (lambda (x)
            (destructuring-bind (key value) x
              (let ((local-key (u:format-symbol :zed "~a" key)))
                (ecase local-key
                  (#.+trait-hook-names+
                   (if (symbolp value)
                       `(,local-key ',value)
                       (error "~s for trait ~s should be an unquoted symbol but got: ~s"
                              key
                              type
                              value)))))))
          options)))))

(defmacro define-internal-trait (type (&key priority) &body (slots . options))
  `(u:eval-always
     (util.oc::define-ordered-class ,type (trait)
       ,slots
       (:order (,@+trait-slot-order+ ,@(mapcar #'car slots)))
       ,@(generate-trait-initargs type priority options))))

(defmacro define-trait (type (&key priority) &body (slots . options))
  `(util.oc::define-ordered-class ,type (trait)
     ,slots
     (:order ,+trait-slot-order+)
     ,@(generate-trait-initargs type priority options)))

(defmacro call-trait-hook (trait hook-type)
  `(with-scope (,(u:format-symbol :keyword "TRAIT-~a-HOOK" hook-type))
     (funcall (fdefinition (,(u:format-symbol :zed "TRAIT-~a-HOOK" hook-type) ,trait)) ,trait)))

;; Create an instance of a trait of the given type. Slow path, for when the type is not a quoted
;; symbol.
(u:fn-> make-trait (context symbol &rest t) trait)
(defun make-trait (context type &rest args)
  (declare (optimize speed))
  (with-allowed-scopes make-trait
      (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
       :trait-attach-hook :trait-detach-hook :trait-physics-hook :trait-update-hook)
    (if (subtypep type 'trait)
        (let ((trait (apply #'make-instance type 'context context args)))
          (call-trait-hook trait :setup)
          trait)
        (error "Trait type ~s is not defined." type))))

;; Create an instance of a trait of the given type. Fast path, for when the type is a quoted symbol.
(define-compiler-macro make-trait (&whole whole context type &rest args)
  (u:with-gensyms (trait)
    (if (and (consp type)
             (eq (car type) 'quote))
        (let ((type (cadr type)))
          (unless (subtypep type 'trait)
            (error "Trait type ~s is not defined." type))
          `(with-allowed-scopes make-trait
               (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
                :trait-attach-hook :trait-detach-hook :trait-physics-hook :trait-update-hook)
             (let ((,trait (make-instance ',type 'context ,context ,@args)))
               (funcall (fdefinition (trait-setup-hook ,trait)) ,trait)
               ,trait)))
        whole)))

(u:fn-> find-trait (game-object symbol) (or trait null))
(defun find-trait (game-object type)
  (declare (optimize speed))
  (u:href (game-object-traits-by-type game-object) type))

(u:fn-> attach-trait (game-object trait) null)
(defun attach-trait (game-object trait)
  (declare (optimize speed))
  (with-allowed-scopes attach-trait
      (:prelude :prefab-instantiate :trait-setup-hook :trait-destroy-hook
       :trait-attach-hook :trait-detach-hook :trait-physics-hook :trait-update-hook)
    (when (trait-owner trait)
      (error "Trait ~s is already attached to a game object." trait))
    (let* ((by-id (game-object-traits-by-id game-object))
           (by-type (game-object-traits-by-type game-object))
           (type (get-trait-type trait))
           (jobs (context-jobs (trait-context trait))))
      (when (u:href by-type type)
        (error "A game object can only have 1 trait of a given type attached to it."))
      (setf (trait-owner trait) game-object
            (u:href by-id trait) trait
            (u:href by-type type) trait)
      (push (list game-object trait #'trait-priority) (jobs-enable-traits jobs))
      (call-trait-hook trait :attach)
      nil)))

(u:fn-> detach-trait (game-object trait) null)
(defun detach-trait (game-object trait)
  (declare (optimize speed))
  (with-allowed-scopes detach-trait
      (:prefab-recompile :trait-setup-hook :trait-destroy-hook :trait-attach-hook
       :trait-detach-hook :trait-physics-hook :trait-update-hook)
    (unless (eq game-object (trait-owner trait))
      (error "Trait ~s is not attached to game object ~s." trait game-object))
    (let ((jobs (context-jobs (trait-context trait))))
      (call-trait-hook trait :detach)
      (push (cons game-object trait) (jobs-disable-traits jobs))
      (setf (trait-owner trait) nil)
      (remhash trait (game-object-traits-by-id game-object))
      (remhash (get-trait-type trait) (game-object-traits-by-type game-object))
      nil)))

(u:fn-> detach-trait-type (game-object symbol) null)
(defun detach-trait-type (game-object type)
  (declare (optimize speed))
  (dolist (trait (u:href (game-object-traits-by-type game-object) type))
    (detach-trait game-object trait)))

(u:fn-> detach-all-traits (game-object) null)
(defun detach-all-traits (game-object)
  (declare (optimize speed))
  (dolist (trait (game-object-trait-order game-object))
    (detach-trait game-object trait)))

(u:fn-> destroy-trait (trait) null)
(defun destroy-trait (trait)
  (declare (optimize speed))
  (with-allowed-scopes destroy-trait
      (:prefab-recompile :trait-setup-hook :trait-destroy-hook :trait-attach-hook
       :trait-detach-hook :trait-physics-hook :trait-update-hook)
    (call-trait-hook trait :destroy)
    (detach-trait (trait-owner trait) trait))
  nil)

(u:fn-> destroy-all-traits (game-object) null)
(defun destroy-all-traits (game-object)
  (declare (optimize speed))
  (dolist (trait (game-object-trait-order game-object))
    (destroy-trait trait))
  nil)
