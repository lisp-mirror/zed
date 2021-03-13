#|
This file is part of the Zed game engine.
Original location: https://git.mfiano.net/mfiano/zed
Copyright © 2021 Michael Fiano mail@mfiano.net.
Licensed under the MIT License.

src/debug.lisp

Description:

This file provides a package and convenience utilities for when the engine is running in debug mode;
not a running a game in production/release mode.

Currently, this includes:

* `check`

A macro that emits an error at runtime if the form passed to it does not return non-NIL and this is
not a release binary. This speeds up some expensive operations, when running in release mode, and
uses slower runtime checks otherwise, which is a convenience for engine developers to guard against
mistakes going un-noticed when shuffling code around.

* `with-debug-group`

A macro that wraps OpenGL API calls when the engine is running in debug mode. This annotates the
calls with convient string names using OpenGL's debug group API. This is a convenience when
debugging games using third-party tools such as RenderDoc, giving a view of application-specific
names for the tree of API calls in the graphical user interface.
|#

(in-package #:cl-user)

(defpackage #:%zed.debug
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.debug)

(defmacro check (&body body)
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
