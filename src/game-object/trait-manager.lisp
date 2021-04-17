(in-package #:zed)

(u:fn-> register-trait (trait) null)
(defun register-trait (trait)
  (declare (optimize speed))
  (let ((manager (context-trait-manager (trait-context trait))))
    (vector-push-extend trait (trait-manager-registered manager))
    nil))

(u:fn-> unregister-trait (trait) null)
(defun unregister-trait (trait)
  (declare (optimize speed))
  (let ((manager (context-trait-manager (trait-context trait))))
    (vector-push-extend trait (trait-manager-unregistered manager))
    nil))

(u:fn-> activate-traits (context) null)
(defun activate-traits (context)
  (declare (optimize speed))
  (let* ((manager (context-trait-manager context))
         (registered (trait-manager-registered manager))
         (active-by-type (trait-manager-active-by-type manager))
         (active-by-id (trait-manager-active-by-id manager)))
    (map nil
         (lambda (x)
           (let ((type (get-trait-type x)))
             (unless (u:href active-by-type type)
               (setf (u:href active-by-type type) (u:dict #'eq)))
             (setf (u:href active-by-type type x) x
                   (u:href active-by-id x) x)))
         registered)
    (setf (fill-pointer registered) 0)
    nil))

(u:fn-> deactivate-traits (context) null)
(defun deactivate-traits (context)
  (declare (optimize speed))
  (let* ((manager (context-trait-manager context))
         (unregistered (trait-manager-unregistered manager))
         (active-by-type (trait-manager-active-by-type manager)))
    (map nil
         (lambda (x)
           (let ((type (get-trait-type x)))
             (remhash x (u:href active-by-type type))
             (remhash x (trait-manager-active-by-id manager))))
         unregistered)
    (setf (fill-pointer unregistered) 0)
    nil))

(defmacro invoke-trait-hook (context hook-type &key game-object)
  (u:with-gensyms (manager by-type type type-table trait)
    `(let* ((,manager (context-trait-manager ,context))
            (,by-type ,(if game-object
                           `(game-object-traits-by-type ,game-object)
                           `(trait-manager-active-by-type ,manager))))
       (dolist (,type (trait-manager-order ,manager))
         ,(if game-object
              `(u:when-let ((,trait (u:href ,by-type ,type)))
                 (call-trait-hook ,trait ,hook-type))
              `(u:when-let ((,type-table (u:href ,by-type ,type)))
                 (u:do-hash-keys (,trait ,type-table)
                   (call-trait-hook ,trait ,hook-type))))))))