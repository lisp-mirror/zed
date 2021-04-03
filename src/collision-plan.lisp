(in-package #:zed)

(defstruct (collision-plan
            (:constructor %make-collision-plan)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (layers nil :type list)
  (table (u:dict #'eq) :type hash-table))

(u:define-printer (collision-plan stream :type nil)
  (format stream "COLLISION-PLAN: ~s" (collision-plan-name collision-plan)))

(glob:define-global-var =collision-plans= (u:dict #'eq))

(defun find-collision-plan (name)
  (or (u:href =collision-plans= name)
      (error "Collision plan ~s is not defined." name)))

(defun update-collision-plan (name mappings)
  (let* ((data (u:href =collision-plans= name))
         (table (collision-plan-table data)))
    (setf (collision-plan-layers data) (remove-duplicates (u:flatten mappings)))
    (clrhash table)
    (dolist (x mappings)
      (destructuring-bind (k v) x
        (when v
          (setf (u:href table k) v))))))

(defun make-collision-plan (name mappings)
  (let ((plan (%make-collision-plan :name name)))
    (setf (u:href =collision-plans= name) plan)
    (update-collision-plan name mappings)
    plan))

(defmacro define-collision-plan (name () &body body)
  `(if (u:href =collision-plans= ',name)
       (update-collision-plan ',name ',body)
       (make-collision-plan ',name ',body)))

(define-collision-plan :default ())
