(in-package #:zed)

(defstruct (mouse-state
            (:predicate nil)
            (:copier nil))
  (x 0 :type u:b16)
  (y 0 :type u:b16)
  (relative-mode-p nil :type boolean)
  (relative-x 0 :type u:b16)
  (relative-y 0 :type u:b16)
  (warp-x 0 :type u:b16)
  (warp-y 0 :type u:b16))

(u:define-constant +mouse-button-names+ #(nil :left :middle :right :x1 :x2) :test #'equalp)

(u:fn-> reset-mouse-state (input-manager) null)
(defun reset-mouse-state (manager)
  (declare (optimize speed))
  (let* ((states (input-manager-states manager))
         (state (u:href states '(:mouse :motion))))
    (setf (u:href states '(:mouse :scroll-horizontal)) 0
          (u:href states '(:mouse :scroll-vertical)) 0
          (mouse-state-relative-x state) 0
          (mouse-state-relative-y state) 0)
    nil))

(u:fn-> input-event/mouse-button-up (input-manager u:ub32) null)
(declaim (inline input-event/mouse-button-up))
(defun input-event/mouse-button-up (manager button)
  (declare (optimize speed))
  (input-transition-out manager :mouse (aref +mouse-button-names+ button))
  (input-transition-out manager :mouse :any)
  nil)

(u:fn-> input-event/mouse-button-down (input-manager u:ub32) null)
(declaim (inline input-event/mouse-button-down))
(defun input-event/mouse-button-down (manager button)
  (declare (optimize speed))
  (input-transition-in manager :mouse (aref +mouse-button-names+ button))
  (input-transition-in manager :mouse :any)
  nil)

(u:fn-> input-event/mouse-wheel-scroll (input-manager u:b32 u:b32) null)
(defun input-event/mouse-wheel-scroll (manager x y)
  (declare (optimize speed))
  (let ((states (input-manager-states manager)))
    (unless (zerop x)
      (setf (u:href states '(:mouse :scroll-horizontal)) x))
    (unless (zerop y)
      (setf (u:href states '(:mouse :scroll-vertical)) y))
    nil))

(u:fn-> input-event/mouse-move (input-manager u:ub16 u:b32 u:b32 u:b32 u:b32) null)
(defun input-event/mouse-move (manager window-height x y relative-x relative-y)
  (declare (optimize speed))
  (let ((state (u:href (input-manager-states manager) '(:mouse :motion)))
        (relative-mode-p (sdl2:relative-mouse-mode-p)))
    (unless relative-mode-p
      (setf (mouse-state-x state) x
            (mouse-state-y state) (- window-height y)))
    (setf (mouse-state-relative-x state) relative-x
          (mouse-state-relative-y state) (- relative-y))
    nil))

(u:fn-> get-mouse-position (context) (values u:b16 u:b16 u:b16 u:b16))
(defun get-mouse-position (context)
  (declare (optimize speed))
  (let* ((state (u:href (input-manager-states (context-input-manager context)) '(:mouse :motion)))
         (x (mouse-state-x state))
         (y (mouse-state-y state))
         (relative-x (mouse-state-relative-x state))
         (relative-y (mouse-state-relative-y state)))
    (values x y relative-x relative-y)))

(u:fn-> get-mouse-scroll (context keyword) u:b32)
(defun get-mouse-scroll (context axis)
  (declare (optimize speed))
  (let ((states (input-manager-states (context-input-manager context))))
    (ecase axis
      (:horizontal (or (u:href states '(:mouse :scroll-horizontal)) 0))
      (:vertical (or (u:href states '(:mouse :scroll-vertical)) 0)))))

(u:fn-> enable-relative-mouse-mode (context) null)
(defun enable-relative-mouse-mode (context)
  (declare (optimize speed))
  (let* ((state (u:href (input-manager-states (context-input-manager context)) '(:mouse :motion)))
         (window-height (window-height (context-window context)))
         (x (mouse-state-x state))
         (y (- window-height (mouse-state-y state))))
    (sdl2:set-relative-mouse-mode 1)
    (setf (mouse-state-relative-mode-p state) t
          (mouse-state-warp-x state) x
          (mouse-state-warp-y state) y)
    nil))

(u:fn-> disable-relative-mouse-mode (context &key (:warp-p boolean)) null)
(defun disable-relative-mouse-mode (context &key (warp-p t))
  (declare (optimize speed))
  (let ((state (u:href (input-manager-states (context-input-manager context)) '(:mouse :motion))))
    (sdl2:set-relative-mouse-mode 0)
    (setf (mouse-state-relative-mode-p state) nil)
    (when warp-p
      (let ((x (mouse-state-warp-x state))
            (y (mouse-state-warp-y state)))
        (sdl2:warp-mouse-in-window nil x y)))
    nil))

(u:fn-> relative-mouse-mode-p (context) boolean)
(defun relative-mouse-mode-p (context)
  (declare (optimize speed))
  (let ((state (u:href (input-manager-states (context-input-manager context)) '(:mouse :motion))))
    (mouse-state-relative-mode-p state)))
