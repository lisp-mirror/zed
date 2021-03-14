(in-package #:cl-user)

(defpackage #:%zed.opengl
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  (:use #:cl))

(in-package #:%zed.opengl)

(u:define-constant +enabled-capabilities+ '(:blend :cull-face :depth-test :dither :multisample)
  :test #'equal)

(u:define-constant +disabled-capabilities+
    '(:clip-distance0 :clip-distance1 :clip-distance2 :clip-distance3 :clip-distance4
      :clip-distance5 :clip-distance6 :clip-distance7 :color-logic-op :debug-output
      :debug-output-synchronous :depth-clamp :framebuffer-srgb :line-smooth :polygon-offset-fill
      :polygon-offset-line :polygon-offset-point :polygon-smooth :primitive-restart
      :primitive-restart-fixed-index :rasterizer-discard :sample-alpha-to-coverage
      :sample-alpha-to-one :sample-coverage :sample-shading :sample-mask :scissor-test :stencil-test
      :texture-cube-map-seamless :program-point-size)
  :test #'equal)

(u:define-constant +blend-mode+ '(:src-alpha :one-minus-src-alpha) :test #'equal)

(u:define-constant +depth-mode+ :less)

(u:define-constant +polygon-mode+ :fill)

;; Prepare the OpenGL context. This must be called before an SDL2 window is created, as it informs
;; SDL2 how the context is to be configured when the window is created.
;; NOTE: This framework only supports OpenGL version 4.3, so we hard-code that here.
(defun prepare-context (&key anti-alias-p)
  (sdl2:gl-set-attrs :context-major-version 4
                     :context-minor-version 3
                     :context-profile-mask 1
                     :doublebuffer 1
                     :multisamplebuffers (if anti-alias-p 1 0)
                     :multisamplesamples (if anti-alias-p 4 0)))

;; Create the OpenGL context. This must be called after the SDL2 window is created to finish up
;; configuring the OpenGL context.
(defun make-context (window-handle)
  (let ((context (sdl2:gl-create-context window-handle)))
    (apply #'gl:enable +enabled-capabilities+)
    (apply #'gl:disable +disabled-capabilities+)
    (apply #'gl:blend-func +blend-mode+)
    (gl:depth-func +depth-mode+)
    (gl:pixel-store :unpack-alignment 1)
    (sdl2:gl-set-swap-interval 1)
    context))

;; Destroy the OpenGL context. This is called during the destruction of the window.
(defun destroy (context)
  (sdl2:gl-delete-context context))
