(in-package #:zed)

(defstruct (collision-system
            (:constructor %make-collision-system)
            (:predicate nil)
            (:copier nil))
  (layers (u:dict #'eq) :type hash-table)
  (multi-level-p t :type boolean)
  (bucket-size 1024 :type u:positive-fixnum)
  (cell-sizes nil :type list)
  (grids (u:dict #'eql) :type hash-table)
  (volume-buffer (make-array 8 :fill-pointer 0 :adjustable t) :type (vector collision-volume)))

(u:define-printer (collision-system stream :type nil)
  (format stream "COLLISION-SYSTEM"))

(u:fn-> make-collision-system (symbol) collision-system)
(defun make-collision-system (plan-name)
  (declare (optimize speed))
  (let* ((plan (find-collision-plan plan-name))
         (multi-level-p (collision-plan-multi-level-p plan))
         (cell-size (collision-plan-cell-size plan))
         (system (%make-collision-system :multi-level-p multi-level-p
                                         :bucket-size (collision-plan-bucket-size plan)
                                         :layers (collision-plan-table plan))))
    (unless multi-level-p
      (register-collision-grid system cell-size))
    system))

(u:fn-> register-collision-grid (collision-system u:positive-fixnum) null)
(defun register-collision-grid (system cell-size)
  (declare (optimize speed))
  (let* ((grids (collision-system-grids system))
         (bucket-size (collision-system-bucket-size system))
         (cell-sizes (collision-system-cell-sizes system))
         (grid (make-hash-grid :bucket-size bucket-size :cell-size cell-size)))
    (unless (u:href grids cell-size)
      (setf (u:href grids cell-size) grid
            (collision-system-cell-sizes system) (sort (list* cell-size cell-sizes) #'<))))
  nil)

(u:fn-> ensure-collision-grid (context collision-volume) null)
(defun ensure-collision-grid (context volume)
  (let ((system (context-collision-system context)))
    (cond
      ((collision-system-multi-level-p system)
       (funcall (collision-volume-update-func volume) volume)
       (v3:with-components ((min- (collision-volume-broad-phase-min volume))
                            (max- (collision-volume-broad-phase-max volume)))
         (let* ((volume-size (max (- max-x min-x) (- max-y min-y) (- max-z min-z)))
                (cell-size (ash 1 (max 3 (integer-length (ceiling volume-size))))))
           (register-collision-grid system cell-size)
           (setf (collision-volume-grid-cell-size volume) cell-size))))
      (t
       (let ((cell-size (car (collision-system-cell-sizes system))))
         (setf (collision-volume-grid-cell-size volume) cell-size))))
    nil))

(u:fn-> register-collision-volume (collision-system collision-volume) null)
(defun register-collision-volume (system volume)
  (declare (optimize speed))
  (let ((cell-size (collision-volume-grid-cell-size volume)))
    (funcall (collision-volume-update-func volume) volume)
    (hash-grid-insert (u:href (collision-system-grids system) cell-size) volume))
  nil)

(u:fn-> compute-volume-contact (collision-volume collision-volume) null)
(defun compute-volume-contact (volume1 volume2)
  (declare (optimize speed))
  (let* ((collide-p (collide-p volume1 volume2))
         (contacts1 (collision-volume-contacts volume1))
         (contacts2 (collision-volume-contacts volume2))
         (contact-p (u:href contacts1 volume2)))
    (cond
      ((and collide-p contact-p)
       ;; TODO: COllision hooks
       #++(on-collision-continue volume1 volume2)
       #++(on-collision-continue volume2 volume1))
      ((and collide-p (not contact-p))
       (setf (u:href contacts1 volume2) t
             (u:href contacts2 volume1) t)
       ;; TODO: Collision hooks
       #++(on-collision-enter volume1 volume2)
       #++(on-collision-enter volume2 volume1))
      ((and (not collide-p) contact-p)
       (remhash volume2 contacts1)
       (remhash volume1 contacts2)
       ;; TODO: Collision hooks
       #++(on-collision-exit volume1 volume2)
       #++(on-collision-exit volume2 volume1)))
    nil))

(u:fn-> merge-collision-grid-bucket (collision-system u:ub32 u:ub32) (vector collision-volume))
(declaim (inline merge-collision-grid-bucket))
(defun merge-collision-grid-bucket (system start-size hash)
  (declare (optimize speed))
  (let ((grids (collision-system-grids system))
        (buffer (collision-system-volume-buffer system)))
    (setf (fill-pointer buffer) 0)
    (u:do-hash (cell-size grid grids)
      (when (>= (the u:ub32 cell-size) start-size)
        (map nil
             (lambda (x)
               (vector-push-extend x buffer))
             (aref (hash-grid-buckets grid) hash))))
    buffer))

(u:fn-> %compute-collisions (hash-table vector) null)
(declaim (inline %compute-collisions))
(defun %compute-collisions (layers bucket)
  (declare (optimize speed))
  (when (>= (length bucket) 2)
    (u:map-combinations
     (lambda (x)
       (let ((volume1 (svref x 0))
             (volume2 (svref x 1)))
         (when (u:href layers
                       (collision-volume-layer volume1)
                       (collision-volume-layer volume2))
           (compute-volume-contact volume1 volume2))))
     bucket
     :copy nil
     :length 2))
  nil)

(u:fn-> compute-collisions/multi-level (collision-system) null)
(declaim (inline compute-collisions/multi-level))
(defun compute-collisions/multi-level (system)
  (declare (optimize speed))
  (let ((grids (collision-system-grids system))
        (layers (collision-system-layers system)))
    (dolist (cell-size (collision-system-cell-sizes system))
      (let* ((grid (u:href grids cell-size))
             (buckets (hash-grid-buckets grid)))
        (dotimes (hash (length buckets))
          (u:when-let* ((local-bucket (aref buckets hash))
                        (bucket (merge-collision-grid-bucket system cell-size hash)))
            (%compute-collisions layers bucket)))
        (map nil (lambda (x) (setf (fill-pointer x) 0)) buckets)
        nil))))

(u:fn-> compute-collisions/flat (collision-system) null)
(declaim (inline compute-collisions/flat))
(defun compute-collisions/flat (system)
  (declare (optimize speed))
  (let* ((layers (collision-system-layers system))
         (grids (collision-system-grids system))
         (grid (u:href grids (car (collision-system-cell-sizes system))))
         (buckets (hash-grid-buckets grid)))
    (dotimes (hash (length buckets))
      (u:when-let ((bucket (aref buckets hash)))
        (%compute-collisions layers bucket)))
    (map nil (lambda (x) (setf (fill-pointer x) 0)) buckets)
    nil))

(u:fn-> compute-collisions (context) null)
(defun compute-collisions (context)
  (declare (optimize speed))
  (let ((system (context-collision-system context)))
    (if (collision-system-multi-level-p system)
        (compute-collisions/multi-level system)
        (compute-collisions/flat system))
    nil))
