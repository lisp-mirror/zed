(in-package #:zed)

(defstruct (collision-plan
            (:constructor %make-collision-plan)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (layers nil :type list)
  (cell-size 128 :type u:positive-fixnum)
  (bucket-size 1024 :type u:positive-fixnum)
  (table (u:dict #'eq) :type hash-table))

(u:define-printer (collision-plan stream :type nil)
  (format stream "COLLISION-PLAN: ~s" (collision-plan-name collision-plan)))

(glob:define-global-var =collision-plans= (u:dict #'eq))

(defun find-collision-plan (name)
  (or (u:href =collision-plans= name)
      (error "Collision plan ~s is not defined." name)))

(defun update-collision-plan (name cell-size bucket-size mappings)
  (let* ((data (u:href =collision-plans= name))
         (table (collision-plan-table data)))
    (setf (collision-plan-cell-size data) cell-size
          (collision-plan-bucket-size data) bucket-size
          (collision-plan-layers data) (remove-duplicates (u:flatten mappings)))
    (clrhash table)
    (dolist (x mappings)
      (destructuring-bind (source targets) x
        (dolist (target targets)
          (unless (u:href table source)
            (setf (u:href table source) (u:dict #'eq)))
          (unless (u:href table target)
            (setf (u:href table target) (u:dict #'eq)))
          (setf (u:href table source target) t
                (u:href table target source) t))))))

(defun make-collision-plan (name cell-size bucket-size mappings)
  (let ((plan (%make-collision-plan :name name)))
    (setf (u:href =collision-plans= name) plan)
    (update-collision-plan name cell-size bucket-size mappings)
    plan))

(defmacro define-collision-plan (name (&key (bucket-size 1024) (cell-size 128)) &body body)
  `(if (u:href =collision-plans= ',name)
       (update-collision-plan ',name ,cell-size ,bucket-size ',body)
       (make-collision-plan ',name ,cell-size ,bucket-size ',body)))

(define-collision-plan :default ())
