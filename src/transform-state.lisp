(in-package #:cl-user)

(defpackage #:%zed.transform-state
  ;; Third-party aliases
  (:local-nicknames
   (#:m4 #:origin.mat4)
   (#:q #:origin.quat)
   (#:u #:golden-utils)
   (#:v3 #:origin.vec3))
  (:use #:cl))

(in-package #:%zed.transform-state)

(declaim (inline %make-state))
(defstruct (state
            (:constructor %make-state)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (translation/current (v3:vec) :type v3:vec)
  (translation/previous (v3:vec) :type v3:vec)
  (translation/incremental (v3:vec) :type v3:vec)
  (translation/incremental-delta (v3:vec) :type v3:vec)
  (translation/interpolated (v3:vec) :type v3:vec)
  (rotation/current (q:quat 1) :type q:quat)
  (rotation/previous (q:quat 1) :type q:quat)
  (rotation/incremental (v3:vec) :type v3:vec)
  (rotation/incremental-delta (q:quat 1) :type q:quat)
  (rotation/interpolated (q:quat 1) :type q:quat)
  (scale/current (v3:vec 1) :type v3:vec)
  (scale/previous (v3:vec 1) :type v3:vec)
  (scale/incremental (v3:vec) :type v3:vec)
  (scale/incremental-delta (v3:vec) :type v3:vec)
  (scale/interpolated (v3:vec) :type v3:vec)
  (local-matrix (m4:mat 1) :type m4:mat)
  (world-matrix (m4:mat 1) :type m4:mat)
  (scaling-matrix (m4:mat 1) :type m4:mat)
  (normal-matrix (m4:mat 1) :type m4:mat))

(u:define-printer (state stream :type nil)
  (format stream "TRANSFORM-STATE"))

(u:fn-> initialize-translation (state &optional (or v3:vec null) (or v3:vec null)) null)
(declaim (inline initialize-translation))
(defun initialize-translation (state &optional initial velocity)
  (declare (optimize speed))
  (when initial
    (setf (translation/current state) initial
          (translation/previous state) (v3:copy initial)))
  (when velocity
    (setf (translation/incremental state) velocity))
  nil)

(u:fn-> initialize-rotation (state &optional (or q:quat null) (or v3:vec null)) null)
(declaim (inline initialize-rotation))
(defun initialize-rotation (state &optional initial velocity)
  (declare (optimize speed))
  (when initial
    (setf (rotation/current state) initial
          (rotation/previous state) (q:copy initial)))
  (when velocity
    (setf (rotation/incremental state) velocity))
  nil)

(u:fn-> initialize-scale (state &optional (or v3:vec real null) (or v3:vec null)) null)
(declaim (inline initialize-scale))
(defun initialize-scale (state &optional initial velocity)
  (declare (optimize speed))
  (when initial
    (let ((initial (etypecase initial
                     (v3:vec initial)
                     (real (v3:vec initial)))))
      (setf (scale/current state) initial
            (scale/previous state) (v3:copy initial))))
  (when velocity
    (setf (scale/incremental state) velocity))
  nil)

(defun make-state (&key translate translate/velocity rotate rotate/velocity scale scale/velocity)
  (declare (optimize speed))
  (let ((state (%make-state)))
    (initialize-translation state translate translate/velocity)
    (initialize-rotation state rotate rotate/velocity)
    (initialize-scale state scale scale/velocity)
    state))

(u:fn-> transform/translation (state u:f32) null)
(declaim (inline transform/translation))
(defun transform/translation (state delta)
  (declare (optimize speed))
  (let ((current (translation/current state))
        (incremental-delta (translation/incremental-delta state)))
    (v3:copy! (translation/previous state) current)
    (v3:scale! incremental-delta (translation/incremental state) delta)
    (v3:+! current current incremental-delta)
    nil))

(u:fn-> transform/rotation (state u:f32) null)
(declaim (inline transform/rotation))
(defun transform/rotation (state delta)
  (declare (optimize speed))
  (let ((current (rotation/current state))
        (incremental-delta (rotation/incremental-delta state)))
    (q:copy! (rotation/previous state) current)
    (q:from-velocity! incremental-delta (rotation/incremental state) delta)
    (q:rotate! current current incremental-delta)
    nil))

(u:fn-> transform/scale (state u:f32) null)
(declaim (inline transform/scale))
(defun transform/scale (state delta)
  (declare (optimize speed))
  (let ((current (scale/current state))
        (incremental-delta (scale/incremental-delta state)))
    (v3:copy! (scale/previous state) current)
    (v3:scale! incremental-delta (scale/incremental state) delta)
    (v3:+! current current incremental-delta)
    nil))
