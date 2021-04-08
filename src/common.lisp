(in-package #:zed)

(glob:define-global-var =system-name= nil)

(deftype fix-sf60 () `(u:f32 ,(- (expt 2f0 60)) ,(expt 2f0 60)))
(deftype fix-df60 () `(u:f64 ,(- (expt 2d0 60)) ,(expt 2d0 60)))
(deftype fix-point () '(simple-array fix-sf60 (3)))

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
  (if (member :sbcl *features*)
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
             (progn ,@body)))
      `(progn
         (warn "Profiling is only supported on SBCL.")
         ,@body)))
