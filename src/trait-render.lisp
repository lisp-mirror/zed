(in-package #:cl-user)

(defpackage #:%zed.trait.render
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:trait #:%zed.trait))
  (:use #:cl))

(in-package #:%zed.trait.render)

(trait::define-trait render ()
  ((%materials :accessor materials
               :initarg :materials)
   (%current-material :accessor current-material
                      :initform nil))
  (:attach-hook #'attach))

(defun attach (trait)
  (print trait))
