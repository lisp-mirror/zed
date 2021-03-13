(in-package #:cl-user)

(defpackage #:%zed.input.window
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:man #:%zed.input.manager)
   (#:mon #:%zed.monitor)
   (#:tr #:%zed.input.transition)
   (#:win #:%zed.window))
  (:use #:cl)
  (:shadow
   #:close))

(in-package #:%zed.input.window)

(u:define-constant +event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore :mouse-focus-enter
      :mouse-focus-exit :keyboard-focus-enter :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(u:fn-> show (man::manager) null)
(declaim (inline show))
(defun show (manager)
  (declare (optimize speed))
  (tr::in manager :window :visible)
  nil)

(u:fn-> hide (man::manager) null)
(declaim (inline hide))
(defun hide (manager)
  (declare (optimize speed))
  (tr::out manager :window :visible)
  nil)

;; TODO: Currently we only set the window's position, but if it is moved to a different monitor, we
;; also want to update the window's monitor reference, since it could have a different resolution,
;; refresh rate, etc.
(u:fn-> move (man::manager win::window u:ub16 u:ub16) null)
(declaim (inline move))
(defun move (manager window x y)
  (declare (optimize speed)
           (ignore manager))
  (win::move window x y)
  nil)

(u:fn-> resize (man::manager win::window u:ub16 u:ub16) null)
(declaim (inline resize))
(defun resize (manager window width height)
  (declare (optimize speed)
           (ignore manager))
  (win::resize window width height)
  nil)

(u:fn-> minimize (man::manager) null)
(defun minimize (manager)
  (declare (optimize speed))
  (let ((states (man::states manager)))
    (cond
      ((u:when-let ((maximize (u:href states '(:window :maximize))))
         (tr::enabled maximize))
       (tr::out manager :window :maximize))
      ((u:when-let ((restore (u:href states '(:window :restore))))
         (tr::enabled restore))
       (tr::out manager :window :restore)))
    (tr::in manager :window :minimize)
    nil))

(u:fn-> maximize (man::manager) null)
(defun maximize (manager)
  (declare (optimize speed))
  (let ((states (man::states manager)))
    (cond
      ((u:when-let ((minimize (u:href states '(:window :minimize))))
         (tr::enabled minimize))
       (tr::out manager :window :minimize))
      ((u:when-let ((restore (u:href states '(:window :restore))))
         (tr::enabled restore))
       (tr::out manager :window :restore)))
    (tr::in manager :window :maximize)
    nil))

(u:fn-> restore (man::manager) null)
(defun restore (manager)
  (declare (optimize speed))
  (let ((states (man::states manager)))
    (cond
      ((u:when-let ((minimize (u:href states '(:window :minimize))))
         (tr::enabled minimize))
       (tr::out manager :window :minimize))
      ((u:when-let ((maximize (u:href states '(:window :maximize))))
         (tr::enabled maximize))
       (tr::out manager :window :maximize)))
    (tr::in manager :window :restore)
    nil))

(u:fn-> mouse-focus-enter (man::manager) null)
(declaim (inline mouse-focus-enter))
(defun mouse-focus-enter (manager)
  (declare (optimize speed))
  (tr::in manager :window :mouse-focus)
  nil)

(u:fn-> mouse-focus-exit (man::manager) null)
(declaim (inline mouse-focus-exit))
(defun mouse-focus-exit (manager)
  (declare (optimize speed))
  (tr::out manager :window :mouse-focus)
  nil)

(u:fn-> keyboard-focus-enter (man::manager) null)
(declaim (inline keyboard-focus-enter))
(defun keyboard-focus-enter (manager)
  (declare (optimize speed))
  (tr::in manager :window :keyboard-focus)
  nil)

(u:fn-> keyboard-focus-exit (man::manager) null)
(declaim (inline keyboard-focus-exit))
(defun keyboard-focus-exit (manager)
  (declare (optimize speed))
  (tr::out manager :window :keyboard-focus)
  nil)

(u:fn-> close (man::manager) null)
(declaim (inline close))
(defun close (manager)
  (declare (optimize speed))
  (tr::in manager :window :close)
  nil)
