(in-package #:cl-user)

(defpackage #:%zed.input.window
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:man #:%zed.input.manager)
   (#:mon #:%zed.render-backend.monitor)
   (#:tr #:%zed.input.transition)
   (#:win #:%zed.render-backend.window))
  (:use #:cl)
  (:shadow
   #:close))

(in-package #:%zed.input.window)

(u:define-constant +event-names+
    #(nil :show :hide nil :move :resize nil :minimize :maximize :restore
      :mouse-focus-enter :mouse-focus-exit :keyboard-focus-enter
      :keyboard-focus-exit :close nil nil)
  :test #'equalp)

(defun show (manager)
  (declare (optimize speed))
  (tr::in manager :window :visible))

(defun hide (manager)
  (declare (optimize speed))
  (tr::out manager :window :visible))

;; TODO: Currently we only set the window's position, but if it is moved to a different monitor, we
;; also want to update the window's monitor reference, since it could have a different resolution,
;; refresh rate, etc.
(defun move (manager window x y)
  (declare (optimize speed)
           (ignore manager))
  (win::move window x y))

(defun resize (manager window width height)
  (declare (optimize speed)
           (ignore manager))
  (win::resize window width height))

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
    (tr::in manager :window :minimize)))

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
    (tr::in manager :window :maximize)))

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
    (tr::in manager :window :restore)))

(defun mouse-focus-enter (manager)
  (declare (optimize speed))
  (tr::in manager :window :mouse-focus))

(defun mouse-focus-exit (manager)
  (declare (optimize speed))
  (tr::out manager :window :mouse-focus))

(defun keyboard-focus-enter (manager)
  (declare (optimize speed))
  (tr::in manager :window :keyboard-focus))

(defun keyboard-focus-exit (manager)
  (declare (optimize speed))
  (tr::out manager :window :keyboard-focus))

(defun close (manager)
  (declare (optimize speed))
  (tr::in manager :window :close))
