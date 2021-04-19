(in-package #:zed)

(defmacro with-continuable ((clock) &body body)
  (u:with-gensyms (entry-time previous-hook)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl *debugger-hook*))
      `(let* ((,previous-hook ,hook)
              (,hook (lambda (condition hook)
                       (declare (ignore hook))
                       (let ((,entry-time (get-clock-time ,clock))
                             (,hook ,previous-hook))
                         (v:debug :zed "Entered debugger")
                         (locally (declare (optimize (speed 1)))
                           (unwind-protect (invoke-debugger condition)
                             (v:debug :zed
                                      "Spent ~3$ seconds in the debugger"
                                      (adjust-clock-debug-time ,clock ,entry-time))
                             nil))))))
         (restart-case (progn ,@body)
           (abort () :report "Zed: Skip processing current frame"))))))

(flet ((generate-live-support-functions ()
         (let ((repl-package (find-if #'find-package '(:slynk :swank))))
           (compile 'send-to-repl
                    (if (eq repl-package :slynk)
                        `(lambda (values &key (comment "Sent from Zed"))
                           (,(find-symbol "COPY-TO-REPL-IN-EMACS" :slynk-mrepl)
                            values :blurb comment :pop-to-buffer nil))
                        (constantly nil))))))
  (generate-live-support-functions))

(defgeneric recompile (type data)
  (:method (type data)
    (error "No live recompilation hook defined for type ~s." type)))
