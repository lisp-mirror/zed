(in-package #:cl-user)

(defpackage #:%zed.input.transition
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:man #:%zed.input.manager))
  (:use #:cl))

(in-package #:%zed.input.transition)

(defstruct (transition
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (enter t :type boolean)
  (enabled t :type boolean)
  (exit nil :type boolean))

(u:fn-> in (man::manager &rest t) null)
(defun in (manager &rest input)
  (declare (optimize speed))
  (let ((states (man::states manager)))
    (u:if-let ((state (u:href states input)))
      (setf (enter state) t
            (enabled state) t
            (exit state) nil)
      (setf (u:href states input) (make-transition)))
    (push input (u:href (man::entering manager) (car input)))
    nil))

(u:fn-> out (man::manager &rest t) null)
(defun out (manager &rest input)
  (declare (optimize speed))
  (u:when-let ((state (u:href (man::states manager) input)))
    (setf (enter state) nil
          (enabled state) nil
          (exit state) t)
    (push input (u:href (man::exiting manager) (car input)))
    nil))

(u:fn-> enable-entering (man::manager) null)
(defun enable-entering (manager)
  (declare (optimize speed))
  (let ((entering (man::entering manager)))
    (u:do-hash-values (type entering)
      (dolist (key type)
        (let ((state (u:href (man::states manager) key)))
          (setf (enter state) nil
                (enabled state) t
                (exit state) nil))))
    (clrhash entering)
    nil))

(u:fn-> disable-exiting (man::manager) null)
(defun disable-exiting (manager)
  (declare (optimize speed))
  (let ((exiting (man::exiting manager)))
    (u:do-hash-values (type exiting)
      (dolist (key type)
        (let ((state (u:href (man::states manager) key)))
          (setf (enter state) nil
                (enabled state) nil
                (exit state) nil))))
    (clrhash exiting)
    nil))
