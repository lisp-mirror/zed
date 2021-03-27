(in-package #:cl-user)

(defpackage #:%zed.spritesheet
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:asset #:%zed.asset)
   (#:ctx #:%zed.context)
   (#:pack #:%zed.pack)
   (#:rc #:%zed.resource-cache)
   (#:sbs #:%zed.shader-buffer-state))
  (:use #:cl)
  (:shadow
   #:find))

(in-package #:%zed.spritesheet)

(defstruct (spritesheet
            (:constructor %make-spritesheet)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (name nil :type list)
  (spec nil :type list)
  (vao 0 :type u:ub16)
  (sprites (u:dict #'equalp) :type hash-table))

(u:fn-> update-buffer (spritesheet) null)
(defun update-buffer (spritesheet)
  (declare (optimize speed))
  (loop :with name = (name spritesheet)
        :with spec = (spec spritesheet)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :of-type fixnum :from 0
        :do (destructuring-bind (&key id x y w h &allow-other-keys) sprite
              (when (and id x y w h)
                (setf (aref pos i) (vector x y)
                      (aref size i) (vector w h)
                      (u:href (sprites spritesheet) id) i)))
        :finally (sbs::write name :path :pos :value pos)
                 (sbs::write name :path :size :value size)))

(u:fn-> find (spritesheet string) u:ub16)
(defun find (spritesheet name)
  (declare (optimize speed))
  (or (u:href (sprites spritesheet) name)
      (error "Sprite ~s not found in spritesheet ~s." name (name spritesheet))))

(defun read-spec-file (asset)
  (asset::with-asset (asset path data :format :lisp)
    data))

(u:fn-> make-spritesheet (ctx::context list list) spritesheet)
(defun make-spritesheet (context asset buffer-spec)
  (declare (optimize speed))
  (rc::with-resource-cache (context :spritesheet asset)
    (let ((spritesheet (%make-spritesheet :name asset
                                          :spec (read-spec-file asset)
                                          :vao (gl:gen-vertex-array))))
      (apply #'sbs::make-buffer (ctx::shader-buffer-state context) asset buffer-spec)
      (update-buffer spritesheet)
      spritesheet)))
