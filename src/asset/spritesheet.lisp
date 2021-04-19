(in-package #:zed)

(defstruct (spritesheet
            (:constructor %make-spritesheet)
            (:predicate nil)
            (:copier nil))
  (name nil :type list)
  (spec nil :type list)
  (vao 0 :type u:ub16)
  (sprites (u:dict #'equalp) :type hash-table))

(u:fn-> update-spritesheet-buffer (spritesheet) null)
(defun update-spritesheet-buffer (spritesheet)
  (declare (optimize speed))
  (loop :with name = (spritesheet-name spritesheet)
        :with spec = (spritesheet-spec spritesheet)
        :with count = (length spec)
        :with pos = (make-array count)
        :with size = (make-array count)
        :for sprite :in spec
        :for i :of-type fixnum :from 0
        :do (destructuring-bind (&key id x y w h &allow-other-keys) sprite
              (when (and id x y w h)
                (setf (aref pos i) (vector x y)
                      (aref size i) (vector w h)
                      (u:href (spritesheet-sprites spritesheet) id) i)))
        :finally (write-shader-buffer name :path :pos :value pos)
                 (write-shader-buffer name :path :size :value size)))

(u:fn-> find-sprite (spritesheet string) u:ub16)
(defun find-sprite (spritesheet name)
  (declare (optimize speed))
  (or (u:href (spritesheet-sprites spritesheet) name)
      (error "Sprite ~s not found in spritesheet ~s." name (spritesheet-name spritesheet))))

(defun read-spritesheet-spec-file (asset)
  (with-asset (asset path data :format :lisp)
    data))

(u:fn-> make-spritesheet (core list list) spritesheet)
(defun make-spritesheet (core asset buffer-spec)
  (declare (optimize speed))
  (with-resource-cache (core :spritesheet asset)
    (let ((spritesheet (%make-spritesheet :name asset
                                          :spec (read-spritesheet-spec-file asset)
                                          :vao (gl:gen-vertex-array))))
      (apply #'make-shader-buffer (core-shader-manager core) asset buffer-spec)
      (update-spritesheet-buffer spritesheet)
      spritesheet)))
