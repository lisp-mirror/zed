(in-package #:cl-user)

(defpackage #:%zed.viewport.manager
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:vp #:%zed.viewport)
   (#:win #:%zed.window))
  (:use #:cl)
  (:shadow
   #:find))

(in-package #:%zed.viewport.manager)

(defstruct (manager
            (:constructor %make-manager)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (window nil :type win::window)
  (table (u:dict #'eq) :type hash-table)
  (default nil :type (or vp::viewport null)))

(u:define-printer (manager stream :type nil)
  (format stream "VIEWPORT-MANAGER"))

(u:fn-> make-manager (win::window) manager)
(defun make-manager (window)
  (declare (optimize speed))
  (let* ((default (vp::make-viewport :default window))
         (manager (%make-manager :window window :default default)))
    (setf (u:href (table manager) :default) default)
    manager))

(u:fn-> register (manager symbol) null)
(defun register (manager name)
  (declare (optimize speed))
  (setf (u:href (table manager) name) (vp::make-viewport name (window manager)))
  nil)

(u:fn-> ensure-viewport (manager symbol) vp::viewport)
(defun ensure-viewport (manager name)
  (declare (optimize speed))
  (unless (find manager name)
    (register manager name))
  (values (find manager name)))

(u:fn-> find (manager symbol) (or vp::viewport null))
(defun find (manager name)
  (declare (optimize speed))
  (u:href (table manager) name))

(u:fn-> find-by-coordinates (manager u:ub16 u:ub16) vp::viewport)
(defun find-by-coordinates (manager x y)
  (declare (optimize speed))
  (u:do-hash-values (v (table manager))
    (let ((vx (vp::x v))
          (vy (vp::y v))
          (vw (vp::width v))
          (vh (vp::height v)))
      (when (and (<= vx x (+ vx vw))
                 (<= vy y (+ vy vh)))
        (return-from find-by-coordinates v))))
  (default manager))

(u:fn-> update (manager) null)
(defun update (manager)
  (declare (optimize speed))
  (u:do-hash-values (v (table manager))
    (vp::update v))
  nil)
