(in-package #:cl-user)

(defpackage #:%zed.core.game-loop
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:clock #:%zed.core.clock)
   (#:ctx #:%zed.core.context)
   (#:live #:%zed.core.live-coding)
   (#:mon #:%zed.render-backend.monitor)
   (#:win #:%zed.render-backend.window))
  (:use #:cl))

(in-package #:%zed.core.game-loop)

;; Return a function that is called periodically to perform necessary book-keeping that does not
;; need to run every frame.
(defun make-periodic-update-function (context)
  (lambda ()
    (live::update-repl (ctx::clock context))))

(defun start (context)
  (declare (optimize speed))
  ;; Hold on to some variables before we enter the main game loop, since they are needed each
  ;; iteration and won't change. This way we won't have to look them up in a busy loop.
  (let* ((clock (ctx::clock context))
         (window (ctx::window context))
         (refresh-rate (mon::get-refresh-rate (win::monitor window)))
         (periodic-func (make-periodic-update-function context)))
    ;; Request the Lisp implementation to perform a full garbage collection immediately before we
    ;; start the main game loop, to mitigate any large amounts of data from initialization or the
    ;; last run from being cleaned up at runtime causing frame drops.
    (tg:gc :full t)
    ;; Actually start the main game loop.
    (u:while (ctx::running-p context)
      (live::with-continuable (clock)
        (clock::tick clock
                     refresh-rate
                     (constantly nil)
                     periodic-func)
        (win::draw window)
        (clock::count-frame clock)))))
