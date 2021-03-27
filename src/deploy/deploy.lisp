(in-package #:zed-deploy)

(defun check-features ()
  #-sbcl
  (error "Deployment is only supported on SBCL.")
  #-zed.release
  (error "You can only deploy by using the zed-deploy system."))

(defun setup (file systems)
  (let ((primary-system (first systems)))
    (if (asdf:find-system primary-system nil)
        (setf util::=system-name= primary-system)
        (error "System ~s could not be found." primary-system))
    ;; TODO: logging
    (log:stop log:*global-controller*)
    (pack::make-pack :path (uiop:pathname-directory-pathname file) :systems systems)
    #+sbcl (sb-ext:disable-debugger)
    #+sbcl (sb-ext:gc :full t)))

(defun dump-image (file top-level)
  (declare (ignorable file top-level))
  #+sbcl
  (apply #'sb-ext:save-lisp-and-die
         file
         :toplevel top-level
         :executable t
         :save-runtime-options t
         :compression 9
         #+windows '(:application-type :gui)
         #-windows nil))

(defun deploy (file systems &rest options)
  (check-features)
  (setup file systems)
  (dump-image file (lambda () (apply #'z:start-game options))))
