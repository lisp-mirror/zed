(in-package #:zed)

(glob:define-global-var =system-name= nil)

(defmacro debug-check (&body body)
  (unless (member :zed.release *features*)
    `(progn
       ,@(mapcar
          (lambda (x)
            `(unless ,x
               (error "Debug check failed: ~s" ',x)))
          body))))

(defmacro with-debug-group (name &body body)
  (if (member :zed.release *features*)
      `(progn ,@body)
      (u:once-only (name)
        `(progn
           (cffi:with-foreign-string (s ,name)
             (%gl:push-debug-group :debug-source-application 0 (length ,name) s))
           (unwind-protect (progn ,@body)
             (%gl:pop-debug-group))))))

(defmacro with-profiling ((enable-p) &body body)
  (let ((packages (remove-if-not
                   (lambda (x)
                     (or (u:string-starts-with-p x "%ZED")
                         (u:string-starts-with-p x "ZED")
                         (u:string-starts-with-p x "CL-OPENGL")
                         (string= x "SHADOW")
                         (string= x "SDL2")))
                   (mapcar #'package-name (list-all-packages)))))
    `(if ,enable-p
         (unwind-protect
              (progn
                (sb-profile:unprofile)
                (sb-profile:profile ,@packages)
                ,@body)
           (sb-profile:report)
           (sb-profile:unprofile)
           (sb-profile:reset))
         (progn ,@body))))
