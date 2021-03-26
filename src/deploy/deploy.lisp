(in-package #:zed-deploy)

(defun check-features ()
  #-sbcl
  (error "Deployment is only supported on SBCL.")
  #-zed.release
  (error "You can only deploy by using the zed-deploy system."))

(defun setup (system-name)
  (if (asdf:find-system system-name nil)
      (setf %zed.util::=system-name= system-name)
      (error "System ~s could not be found." system-name))
  ;; TODO: logging
  ;; (log:stop log:*global-controller*)
  #+sbcl (sb-ext:disable-debugger)
  #+sbcl (sb-ext:gc :full t))

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

(defun deploy (system-name file &rest options)
  (check-features)
  (setup system-name)
  (dump-image file (lambda () (apply #'z:start-game options))))
