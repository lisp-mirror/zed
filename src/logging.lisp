(in-package #:zed)

(defun start-logging (config)
  (unless (verbose:thread verbose:*global-controller*)
    (verbose:start verbose:*global-controller*))
  (setf (verbose:repl-level) (config-log-repl-level config)
        (verbose:repl-categories) (config-log-repl-categories config))
  #+zed.release
  (let ((log-file (uiop:merge-pathnames*
                   (format nil "~(~a~).log" =system-name=)
                   #.(uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))))
    (verbose:define-pipe ()
      (verbose:level-filter :level :error)
      (verbose:rotating-file-faucet :template log-file))))

(defun stop-logging ()
  (verbose:sync))
