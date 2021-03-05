(in-package #:cl-user)

(defpackage #:%zed.input.mouse
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:man #:%zed.input.manager)
   (#:tr #:%zed.input.transition))
  (:use #:cl))

(in-package #:%zed.input.mouse)

(defstruct (state
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (x 0 :type u:b16)
  (y 0 :type u:b16)
  (relative-mode-p nil :type boolean)
  (relative-x 0 :type u:b16)
  (relative-y 0 :type u:b16)
  (warp-x 0 :type u:b16)
  (warp-y 0 :type u:b16))

(u:define-constant +button-names+
    #(nil :left :middle :right :x1 :x2)
  :test #'equalp)

(defun reset-state (manager)
  (declare (optimize speed))
  (let* ((states (man::states manager))
         (state (u:href states '(:mouse :motion))))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0
          (relative-x state) 0
          (relative-y state) 0)))

(defun button-up (manager button)
  (declare (optimize speed))
  (tr::out manager :mouse (aref +button-names+ button))
  (tr::out manager :mouse :any))

(defun button-down (manager button)
  (declare (optimize speed))
  (tr::in manager :mouse (aref +button-names+ button))
  (tr::in manager :mouse :any))

(defun wheel (manager x y)
  (declare (optimize speed)
           (u:b32 x y))
  (let ((states (man::states manager)))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))))

(defun move (manager window-height x y relative-x relative-y)
  (declare (optimize speed)
           (u:ub16 window-height)
           (u:b32 x y relative-x relative-y))
  (let ((state (u:href (man::states manager) '(:mouse :motion)))
        (relative-mode-p (sdl2:relative-mouse-mode-p)))
    (unless relative-mode-p
      (setf (x state) x
            (y state) (- window-height y)))
    (setf (relative-x state) relative-x
          (relative-y state) (- relative-y))))
