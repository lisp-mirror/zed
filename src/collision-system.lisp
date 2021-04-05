(in-package #:zed)

(defstruct (collision-system
            (:constructor %make-collision-system)
            (:predicate nil)
            (:copier nil))
  (plan nil :type collision-plan)
  (registered (u:dict #'eq) :type hash-table)
  (deregistered (u:dict #'eq) :type hash-table)
  (active (u:dict #'eq) :type hash-table)
  (contacts (u:dict #'eq) :type hash-table)
  (buffer (make-array 8 :fill-pointer 0 :adjustable t) :type vector))

(u:define-printer (collision-system stream :type nil)
  (format stream "COLLISION-SYSTEM"))

(u:fn-> make-collision-system (symbol) collision-system)
(defun make-collision-system (plan-name)
  (declare (optimize speed))
  (let ((system (%make-collision-system :plan (find-collision-plan plan-name))))
    (dolist (layer (collision-plan-layers (collision-system-plan system)))
      (setf (u:href (collision-system-registered system) layer) (u:dict #'eq)
            (u:href (collision-system-deregistered system) layer) (u:dict #'eq)
            (u:href (collision-system-active system) layer) (u:dict #'eq)))
    system))

(u:fn-> collider-contact-p (collision-system trait trait) (or tr.collider:collider null))
(defun collider-contact-p (system collider1 collider2)
  (declare (optimize speed))
  (let ((contacts (collision-system-contacts system)))
    (when (u:href contacts collider1)
      (u:href contacts collider1 collider2))))

(u:fn-> collider-contact-enter (collision-system trait trait) null)
(defun collider-contact-enter (system collider1 collider2)
  (declare (optimize speed))
  (let ((contacts (collision-system-contacts system)))
    (unless (u:href contacts collider1)
      (setf (u:href contacts collider1) (u:dict #'eq)))
    (setf (u:href contacts collider1 collider2) collider2)
    (unless (u:href contacts collider2)
      (setf (u:href contacts collider2) (u:dict #'eq)))
    (setf (u:href contacts collider2 collider1) collider1)
    (tr.collider::enter collider1 collider2)
    (tr.collider::enter collider2 collider1))
  nil)

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
    (tr.collider::exit collider1 collider2)
    (tr.collider::exit collider2 collider1))
  nil)

(u:fn-> collider-contact-continue (trait trait) null)
(defun collider-contact-continue (collider1 collider2)
  (declare (optimize speed))
  (tr.collider::continue collider1 collider2)
  (tr.collider::continue collider2 collider1)
  nil)

(u:fn-> remove-collider-contacts (collision-system trait) null)
(defun remove-collider-contacts (system collider)
  (declare (optimize speed))
  (u:when-let* ((contacts (collision-system-contacts system))
                (colliders (u:href contacts collider)))
    (u:do-hash-keys (k colliders)
      (when (collider-contact-p system collider k)
        (collider-contact-exit system collider k)))
    nil))

(u:fn-> register-collider (trait symbol) null)
(defun register-collider (collider layer)
  (declare (optimize speed))
  (let* ((system (context-collision-system (trait-context collider)))
         (registered (collision-system-registered system)))
    (unless (u:href registered layer)
      (error "Collider layer ~s is used but not defined in the collision plan." layer))
    (setf (u:href registered layer collider) collider)
    nil))

(u:fn-> deregister-collider (trait symbol) null)
(defun deregister-collider (collider layer)
  (declare (optimize speed))
  (let* ((system (context-collision-system (trait-context collider)))
         (deregistered (collision-system-deregistered system)))
    (remove-collider-contacts system collider)
    (setf (u:href deregistered layer collider) collider)
    nil))

(u:fn-> compute-collider-contact (collision-system trait trait) null)
(defun compute-collider-contact (system collider1 collider2)
  (declare (optimize speed))
  (u:when-let ((volume1 (tr.collider::volume collider1))
               (volume2 (tr.collider::volume collider2)))
    (let ((collided-p (collide-p volume1 volume2))
          (contact-p (collider-contact-p system collider1 collider2)))
      (cond
        ((and collided-p contact-p)
         (collider-contact-continue collider1 collider2))
        ((and collided-p (not contact-p))
         (collider-contact-enter system collider1 collider2))
        ((and (not collided-p) contact-p)
         (collider-contact-exit system collider1 collider2))))))

(u:fn-> compute-collisions/active (collision-system) null)
(defun compute-collisions/active (system)
  (let* ((active (collision-system-active system))
         (buffer (collision-system-buffer system))
         (plan (collision-system-plan system))
         (table (collision-plan-table plan)))
    (dolist (collider1-layer (collision-plan-layers plan))
      (dolist (collider2-layer (u:href table collider1-layer))
        (if (eq collider1-layer collider2-layer)
            (u:when-let ((colliders (u:href active collider1-layer)))
              (setf (fill-pointer buffer) 0)
              (u:do-hash-keys (k colliders)
                (vector-push-extend k buffer))
              (when (>= (length buffer) 2)
                (u:map-combinations
                 (lambda (x)
                   (compute-collider-contact system (aref x 0) (aref x 1)))
                 buffer
                 :length 2
                 :copy nil)))
            (u:do-hash-keys (k1 (u:href active collider1-layer))
              (u:do-hash-keys (k2 (u:href active collider2-layer))
                (compute-collider-contact system k1 k2))))))))

(u:fn-> compute-collisions/registered (collision-system) null)
(defun compute-collisions/registered (system)
  (declare (optimize speed))
  (let* ((active (collision-system-active system))
         (registered (collision-system-registered system))
         (plan (collision-system-plan system))
         (table (collision-plan-table plan)))
    (dolist (c1-layer (collision-plan-layers plan))
      (let ((layer-registered (u:href registered c1-layer)))
        (u:do-hash-keys (c1 layer-registered)
          (remhash c1 layer-registered)
          (unless (u:href active c1)
            (let ((c2-layers (u:href table c1-layer)))
              (dolist (c2-layer c2-layers)
                (u:do-hash-keys (c2 (u:href active c2-layer))
                  (compute-collider-contact system c1 c2)))
              (setf (u:href active c1-layer c1) c1))))))))

(u:fn-> compute-collisions/deregistered (collision-system) null)
(defun compute-collisions/deregistered (system)
  (declare (optimize speed))
  (let* ((active (collision-system-active system))
         (registered (collision-system-registered system))
         (deregistered (collision-system-deregistered system)))
    (dolist (layer (collision-plan-layers (collision-system-plan system)))
      (let ((layer-deregistered (u:href deregistered layer)))
        (unless (zerop (hash-table-count layer-deregistered))
          (u:do-hash-keys (k layer-deregistered)
            (remhash k (u:href active layer))
            (remhash k (u:href registered layer))
            (remove-collider-contacts system k)
            (remhash k layer-deregistered)))))))

(u:fn-> compute-collisions (context) null)
(defun compute-collisions (context)
  (declare (optimize speed))
  (let ((system (context-collision-system context)))
    (compute-collisions/deregistered system)
    (compute-collisions/registered system)
    (compute-collisions/active system)
    nil))