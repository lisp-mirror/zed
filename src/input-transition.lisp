(in-package #:zed)

(defstruct (input-transition
            (:predicate nil)
            (:copier nil))
  (enter t :type boolean)
  (enabled t :type boolean)
  (exit nil :type boolean))

(u:fn-> input-transition-in (input-manager &rest t) null)
(defun input-transition-in (manager &rest input)
  (declare (optimize speed))
  (let ((states (input-manager-states manager)))
    (u:if-let ((state (u:href states input)))
      (setf (input-transition-enter state) t
            (input-transition-enabled state) t
            (input-transition-exit state) nil)
      (setf (u:href states input) (make-input-transition)))
    (push input (u:href (input-manager-entering manager) (car input)))
    nil))

(u:fn-> input-transition-out (input-manager &rest t) null)
(defun input-transition-out (manager &rest input)
  (declare (optimize speed))
  (u:when-let ((state (u:href (input-manager-states manager) input)))
    (setf (input-transition-enter state) nil
          (input-transition-enabled state) nil
          (input-transition-exit state) t)
    (push input (u:href (input-manager-exiting manager) (car input)))
    nil))

(u:fn-> input-enable-entering (input-manager) null)
(defun input-enable-entering (manager)
  (declare (optimize speed))
  (let ((entering (input-manager-entering manager)))
    (u:do-hash-values (type entering)
      (dolist (key type)
        (let ((state (u:href (input-manager-states manager) key)))
          (setf (input-transition-enter state) nil
                (input-transition-enabled state) t
                (input-transition-exit state) nil))))
    (clrhash entering)
    nil))

(u:fn-> input-disable-exiting (input-manager) null)
(defun input-disable-exiting (manager)
  (declare (optimize speed))
  (let ((exiting (input-manager-exiting manager)))
    (u:do-hash-values (type exiting)
      (dolist (key type)
        (let ((state (u:href (input-manager-states manager) key)))
          (setf (input-transition-enter state) nil
                (input-transition-enabled state) nil
                (input-transition-exit state) nil))))
    (clrhash exiting)
    nil))
