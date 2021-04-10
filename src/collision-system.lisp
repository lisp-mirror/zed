(in-package #:zed)

(defstruct (collision-system
            (:constructor %make-collision-system)
            (:predicate nil)
            (:copier nil))
  (layers (u:dict #'eq) :type hash-table)
  (bucket-size 1024 :type u:positive-fixnum)
  (cell-sizes nil :type list)
  (grids (u:dict #'eql) :type hash-table)
  (volume-buffer (make-array 8 :fill-pointer 0 :adjustable t) :type (vector collision-volume))
  (visited-volumes (u:dict #'eq) :type hash-table)
  (contacts (u:dict #'eq) :type hash-table))

(u:define-printer (collision-system stream :type nil)
  (format stream "COLLISION-SYSTEM"))

(u:fn-> make-collision-system (symbol) collision-system)
(defun make-collision-system (plan-name)
  (declare (optimize speed))
  (let ((plan (find-collision-plan plan-name)))
    (%make-collision-system :bucket-size (collision-plan-bucket-size plan)
                            :layers (collision-plan-table plan))))

(u:fn-> register-collider (collision-system u:positive-fixnum collision-volume) null)
(defun register-collider (system cell-size volume)
  (declare (optimize speed))
  (hash-grid-insert (u:href (collision-system-grids system) cell-size) volume)
  nil)

(u:fn-> collider-contact-p (collision-system trait trait) (or trait null))
(defun collider-contact-p (system collider1 collider2)
  (declare (optimize speed))
  (let ((contacts (collision-system-contacts system)))
    (u:when-let ((contact (u:href contacts collider1)))
      (u:href contact collider2))))

(u:fn-> collider-contact-enter (collision-system trait trait) null)
(defun collider-contact-enter (system collider1 collider2)
  (declare (optimize speed))
  (let ((contacts (collision-system-contacts system)))
    (u:if-let ((contact (u:href contacts collider1)))
      (setf (u:href contact collider2) collider2)
      (setf (u:href contacts collider1) (u:dict #'eq collider2 collider2)))
    (u:if-let ((contact (u:href contacts collider2)))
      (setf (u:href contact collider1) collider1)
      (setf (u:href contacts collider2) (u:dict #'eq collider1 collider1)))
    (tr.col::enter collider1 collider2)
    (tr.col::enter collider2 collider1)
    nil))

(u:fn-> collider-contact-exit (collision-system trait trait) null)
(defun collider-contact-exit (system collider1 collider2)
  (declare (optimize speed))
  (let ((contacts (collision-system-contacts system)))
    (u:when-let ((table2 (u:href contacts collider1)))
      (remhash collider2 table2)
      (when (zerop (hash-table-count table2))
        (remhash collider1 contacts)))
    (u:when-let ((table1 (u:href contacts collider2)))
      (remhash collider1 table1)
      (when (zerop (hash-table-count table1))
        (remhash collider2 contacts)))
    (tr.col::exit collider1 collider2)
    (tr.col::exit collider2 collider1)
    nil))

(u:fn-> collider-contact-continue (trait trait) null)
(defun collider-contact-continue (collider1 collider2)
  (declare (optimize speed))
  (tr.col::continue collider1 collider2)
  (tr.col::continue collider2 collider1)
  nil)

(u:fn-> compute-collider-contact (collision-system trait trait) null)
(defun compute-collider-contact (system collider1 collider2)
  (declare (optimize speed))
  (u:when-let ((volume1 (tr.col::volume collider1))
               (volume2 (tr.col::volume collider2)))
    (let ((collided-p (collide-p volume1 volume2))
          (contact-p (collider-contact-p system collider1 collider2)))
      (cond
        ((and collided-p contact-p)
         (collider-contact-continue collider1 collider2))
        ((and collided-p (not contact-p))
         (collider-contact-enter system collider1 collider2))
        ((and (not collided-p) contact-p)
         (collider-contact-exit system collider1 collider2))))))

(u:fn-> merge-collision-grid-bucket (collision-system u:ub32 u:ub32) (vector collision-volume))
(declaim (inline merge-collision-grid-bucket))
(defun merge-collision-grid-bucket (system start-size hash)
  (declare (optimize speed))
  (let ((grids (collision-system-grids system))
        (visited (collision-system-visited-volumes system))
        (buffer (collision-system-volume-buffer system)))
    (clrhash visited)
    (setf (fill-pointer buffer) 0)
    (u:do-hash (cell-size grid grids)
      (when (>= (the u:ub32 cell-size) start-size)
        (dolist (volume (aref (hash-grid-buckets grid) hash))
          (unless (u:href visited volume)
            (vector-push-extend volume buffer)
            (setf (u:href visited volume) t)))))
    buffer))

(u:fn-> compute-collisions (context) null)
(defun compute-collisions (context)
  (declare (optimize speed))
  (let* ((system (context-collision-system context))
         (grids (collision-system-grids system))
         (layers (collision-system-layers system)))
    (dolist (cell-size (collision-system-cell-sizes system))
      (let* ((grid (u:href grids cell-size))
             (buckets (hash-grid-buckets grid)))
        (loop :for hash :of-type fixnum :from 0
              :for local-bucket :across buckets
              :when local-bucket
                :do (let ((bucket (merge-collision-grid-bucket system cell-size hash)))
                      (when (>= (length bucket) 2)
                        (u:map-combinations
                         (lambda (x)
                           (let ((collider1 (collision-volume-collider (svref x 0)))
                                 (collider2 (collision-volume-collider (svref x 1))))
                             (when (u:href layers
                                           (tr.col::layer collider1)
                                           (tr.col::layer collider2))
                               (compute-collider-contact system collider1 collider2))))
                         bucket
                         :copy nil
                         :length 2))))
        (fill buckets nil)))))
