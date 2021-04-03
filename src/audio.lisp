(in-package #:zed)

(defun start-audio ()
  (sdl2:init* '(:audio))
  (sdl2-mixer:init :flac :ogg)
  (sdl2-mixer:open-audio 44100 :s16sys 2 1024)
  (sdl2-mixer:allocate-channels 16)
  (v:info :zed "Started audio system"))

(defun stop-audio ()
  (sdl2-mixer:halt-channel -1)
  (sdl2-mixer:close-audio)
  (sdl2-mixer:quit)
  (v:info :zed "Stopped audio system"))
