(in-package #:cl-user)

(defpackage #:%zed.input.transition
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:in.mgr #:%zed.input.manager))
  (:use #:cl))

(in-package #:%zed.input.transition)

(defstruct (transition
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (enter t :type boolean)
  (enabled t :type boolean)
  (exit nil :type boolean))

(u:fn-> in (in.mgr::manager &rest t) null)
(defun in (manager &rest input)
  (declare (optimize speed))
  (let ((states (in.mgr::states manager)))
    (u:if-let ((state (u:href states input)))
      (setf (enter state) t
            (enabled state) t
            (exit state) nil)
      (setf (u:href states input) (make-transition)))
    (push input (u:href (in.mgr::entering manager) (car input)))
    nil))

(u:fn-> out (in.mgr::manager &rest t) null)
(defun out (manager &rest input)
  (declare (optimize speed))
  (u:when-let ((state (u:href (in.mgr::states manager) input)))
    (setf (enter state) nil
          (enabled state) nil
          (exit state) t)
    (push input (u:href (in.mgr::exiting manager) (car input)))
    nil))

(u:fn-> enable-entering (in.mgr::manager) null)
(defun enable-entering (manager)
  (declare (optimize speed))
  (let ((entering (in.mgr::entering manager)))
    (u:do-hash-values (type entering)
      (dolist (key type)
        (let ((state (u:href (in.mgr::states manager) key)))
          (setf (enter state) nil
                (enabled state) t
                (exit state) nil))))
    (clrhash entering)
    nil))

(u:fn-> disable-exiting (in.mgr::manager) null)
(defun disable-exiting (manager)
  (declare (optimize speed))
  (let ((exiting (in.mgr::exiting manager)))
    (u:do-hash-values (type exiting)
      (dolist (key type)
        (let ((state (u:href (in.mgr::states manager) key)))
          (setf (enter state) nil
                (enabled state) nil
                (exit state) nil))))
    (clrhash exiting)
    nil))
