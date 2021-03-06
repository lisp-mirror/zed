(in-package #:cl-user)

;;;; The base module contains everything that needs to be loaded early on, because they are used
;;;; throughout the rest of the modules.

(defpackage #:%zed.base
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames)
  (:use #:cl))
