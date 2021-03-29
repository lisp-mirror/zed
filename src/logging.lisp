(in-package #:cl-user)

(defpackage #:%zed.logging
  ;; Third-party packages
  (:local-nicknames
   (#:log #:verbose)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:cfg #:%zed.config)
   (#:util #:%zed.util))
  (:shadowing-import-from
   #:verbose
   #:trace
   #:debug
   #:info
   #:error)
  (:use #:cl))

(in-package #:%zed.logging)

(defun start (config)
  (unless (log:thread log:*global-controller*)
    (log:start log:*global-controller*))
  (setf (log:repl-level) (cfg::log-repl-level config)
        (log:repl-categories) (cfg::log-repl-categories config))
  #+zed.release
  (let ((log-file (uiop:merge-pathnames*
                   (format nil "~(~a~).log" util::=system-name=)
                   #.(uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))))
    (log:define-pipe ()
      (log:level-filter :level :error)
      (log:rotating-file-faucet :template log-file))))

(defun stop ()
  (log:sync))
