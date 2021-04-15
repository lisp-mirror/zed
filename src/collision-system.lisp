(in-package #:zed)

(defstruct (collision-system
            (:constructor %make-collision-system)
            (:predicate nil)
            (:copier nil))
  (layers (u:dict #'eq) :type hash-table)
  (grid nil :type hash-grid)
  (contact-table (u:dict #'eq) :type hash-table)
  (picker (line3d:line :start (p3:point 0.0 0.0 0.0) :end (p3:point 0.0 0.0 0.0))
   :type line3d:line))

(u:define-printer (collision-system stream :type nil)
  (format stream "COLLISION-SYSTEM"))

(u:fn-> make-collision-system (symbol) collision-system)
(defun make-collision-system (plan-name)
  (declare (optimize speed))
  (let* ((plan (find-collision-plan plan-name))
         (cell-size (collision-plan-cell-size plan))
         (bucket-size (collision-plan-bucket-size plan))
         (grid (make-hash-grid :cell-size cell-size :bucket-size bucket-size)))
    (%make-collision-system :grid grid
                            :layers (collision-plan-table plan))))

(u:fn-> register-collision-volume (collision-system collision-volume) null)
(defun register-collision-volume (system volume)
  (declare (optimize speed))
  (funcall (collision-volume-update-func volume) volume)
  (hash-grid-insert (collision-system-grid system) volume)
  nil)

(defgeneric on-collision-enter (layer1 source1 layer2 source2)
  (:method (layer1 source1 layer2 source2)))

(defgeneric on-collision-continue (layer1 source1 layer2 source2)
  (:method (layer1 source1 layer2 source2)))

(defgeneric on-collision-exit (layer1 source1 layer2 source2)
  (:method (layer1 source1 layer2 source2)))

(defgeneric on-game-object-picked (layer source)
  (:method (layer source)))

(defmacro define-collision-hook (hook layer-spec &body body)
  (u:with-gensyms (layer1-symbol layer2-symbol)
    (case hook
      ((:enter :continue :exit)
       (destructuring-bind ((source1 layer1) (source2 layer2)) layer-spec
         `(defmethod ,(u:format-symbol :zed "ON-COLLISION-~a" hook)
              ((,layer1-symbol (eql ',layer1)) ,source1
               (,layer2-symbol (eql ',layer2)) ,source2)
            (with-scope (:collision-hook)
              ,@body))))
      (t
       `(error "Invalid collision hook type: ~s." ',hook)))))

(u:fn-> %on-collision-enter (collision-volume collision-volume) null)
(declaim (inline %on-collision-enter))
(defun %on-collision-enter (volume1 volume2)
  (declare (optimize speed))
  (let ((layer1 (collision-volume-layer volume1))
        (layer2 (collision-volume-layer volume2))
        (source1 (collision-volume-source volume1))
        (source2 (collision-volume-source volume2)))
    (on-collision-enter layer1 source1 layer2 source2)
    (on-collision-enter layer2 source2 layer1 source1))
  nil)

(u:fn-> %on-collision-continue (collision-volume collision-volume) null)
(declaim (inline %on-collision-continue))
(defun %on-collision-continue (volume1 volume2)
  (declare (optimize speed))
  (let ((layer1 (collision-volume-layer volume1))
        (layer2 (collision-volume-layer volume2))
        (source1 (collision-volume-source volume1))
        (source2 (collision-volume-source volume2)))
    (on-collision-continue layer1 source1 layer2 source2)
    (on-collision-continue layer2 source2 layer1 source1))
  nil)

(u:fn-> %on-collision-exit (collision-volume collision-volume) null)
(declaim (inline %on-collision-exit))
(defun %on-collision-exit (volume1 volume2)
  (declare (optimize speed))
  (let ((layer1 (collision-volume-layer volume1))
        (layer2 (collision-volume-layer volume2))
        (source1 (collision-volume-source volume1))
        (source2 (collision-volume-source volume2)))
    (remhash volume1 (collision-volume-contacts volume2))
    (remhash volume2 (collision-volume-contacts volume1))
    (on-collision-exit layer1 source1 layer2 source2)
    (on-collision-exit layer2 source2 layer1 source1))
  nil)

(u:fn-> compute-volume-contact (collision-system collision-volume collision-volume) null)
(defun compute-volume-contact (system volume1 volume2)
  (declare (optimize speed))
  (u:when-let ((collide-p (collide-p volume1 volume2)))
    (let* ((contacts1 (collision-volume-contacts volume1))
           (contact-p (u:href contacts1 volume2)))
      (cond
        ((and collide-p (not contact-p))
         (setf (u:href contacts1 volume2) t
               (u:href (collision-volume-contacts volume2) volume1) t
               (u:href (collision-system-contact-table system) volume1) volume2
               (u:href (collision-system-contact-table system) volume2) volume1)
         (%on-collision-enter volume1 volume2))
        ((and collide-p contact-p)
         (%on-collision-continue volume1 volume2)))
      nil)))

(u:fn-> compute-volume-separations (collision-system) null)
(defun compute-volume-separations (system)
  (declare (optimize speed))
  (let ((contacts (collision-system-contact-table system)))
    (u:do-hash (volume1 volume2 contacts)
      (unless (and
               (collide-p volume1 volume2)
               (trait-owner (collision-volume-collider volume1))
               (trait-owner (collision-volume-collider volume2)))
        (remhash volume1 contacts)
        (remhash volume2 contacts)
        (%on-collision-exit volume1 volume2)))
    nil))

(u:fn-> %compute-collisions (collision-system hash-table vector) null)
(declaim (inline %compute-collisions))
(defun %compute-collisions (system layers bucket)
  (declare (optimize speed))
  (when (>= (length bucket) 2)
    (u:map-combinations
     (lambda (x)
       (let ((volume1 (svref x 0))
             (volume2 (svref x 1)))
         (u:when-let ((layer (u:href layers (collision-volume-layer volume1))))
           (when (u:href layer (collision-volume-layer volume2))
             (compute-volume-contact system volume1 volume2)))))
     bucket
     :length 2
     :copy nil)
    nil))

(u:fn-> compute-collisions (context) null)
(defun compute-collisions (context)
  (declare (optimize speed))
  (let* ((system (context-collision-system context))
         (layers (collision-system-layers system)))
    (map nil
         (lambda (x)
           (%compute-collisions system layers x)
           (setf (fill-pointer x) 0))
         (hash-grid-buckets (collision-system-grid system)))
    (compute-volume-separations system)
    nil))
