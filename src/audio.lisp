(in-package #:cl-user)

(defpackage #:%zed.audio
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:log #:%zed.logging))
  (:use #:cl))

(in-package #:%zed.audio)

(defun start ()
  (sdl2:init* '(:audio))
  (sdl2-mixer:init :flac :ogg)
  (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
  (sdl2-mixer:allocate-channels 16)
  (log::info :zed.audio "Started audio system"))

(defun stop ()
  (sdl2-mixer:halt-channel -1)
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  (log::info :zed.audio "Stopped audio system"))
