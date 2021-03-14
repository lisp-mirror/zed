(in-package #:cl-user)

(defpackage #:%zed.live-coding
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:clock #:%zed.clock))
  (:use #:cl))

(in-package #:%zed.live-coding)

(defmacro with-continuable ((clock) &body body)
  (u:with-gensyms (entry-time previous-hook)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl *debugger-hook*))
      `(let* ((,previous-hook ,hook)
              (,hook (lambda (condition hook)
                       (declare (ignore hook))
                       (let ((,entry-time (clock::get-time ,clock))
                             (,hook ,previous-hook))
                         (format t "Entered debugger.~%")
                         (unwind-protect (invoke-debugger condition)
                           (format t "Spent ~3$ seconds in the debugger.~%"
                                   (clock::adjust-pause-time ,clock ,entry-time)))))))
         (restart-case (progn ,@body)
           (abort () :report "Zed: Skip processing current frame"))))))

(flet ((generate-live-support-functions ()
         (let ((repl-package (find-if #'find-package '(:slynk :swank))))
           (compile 'setup-repl
                    (if (eq repl-package :slynk)
                        `(lambda ()
                           (,(find-symbol "SEND-PROMPT" :slynk-mrepl)))
                        (constantly nil)))
           (compile 'update-repl
                    (case repl-package
                      (:slynk
                       `(lambda (clock)
                          (let ((before-time (clock::get-time clock)))
                            (,(find-symbol "PROCESS-REQUESTS" :slynk) t)
                            (clock::adjust-pause-time clock before-time))))
                      (:swank
                       `(lambda (clock)
                          (u:when-let ((repl (or ,(find-symbol "*EMACS-CONNECTION*" :swank)
                                                 (,(find-symbol "DEFAULT-CONNECTION" :swank))))
                                       (before-time (get-time)))
                            (,(find-symbol "HANDLE-REQUESTS" :swank) repl t)
                            (clock::adjust-pause-time clock before-time))))
                      (t (constantly nil))))
           (compile 'send-to-repl
                    (if (eq repl-package :slynk)
                        `(lambda (values &key (comment "Sent from Zed"))
                           (,(find-symbol "COPY-TO-REPL-IN-EMACS" :slynk-mrepl)
                            values :blurb comment :pop-to-buffer nil))
                        (constantly nil))))))
  (generate-live-support-functions))
