(in-package #:zed)

(declaim (inline %make-transform-state))
(defstruct (transform-state
            (:constructor %make-transform-state)
            (:predicate nil)
            (:copier nil))
  (translation/current (v3:zero) :type v3:vec)
  (translation/previous (v3:zero) :type v3:vec)
  (translation/incremental (v3:zero) :type v3:vec)
  (translation/incremental-delta (v3:zero) :type v3:vec)
  (translation/interpolated (v3:zero) :type v3:vec)
  (rotation/current (q:id) :type q:quat)
  (rotation/previous (q:id) :type q:quat)
  (rotation/incremental (v3:zero) :type v3:vec)
  (rotation/incremental-delta (q:id) :type q:quat)
  (rotation/interpolated (q:id) :type q:quat)
  (scale/current (v3:ones) :type v3:vec)
  (scale/previous (v3:ones) :type v3:vec)
  (scale/incremental (v3:zero) :type v3:vec)
  (scale/incremental-delta (v3:zero) :type v3:vec)
  (scale/interpolated (v3:zero) :type v3:vec)
  (local-matrix (m4:id) :type m4:mat)
  (world-matrix (m4:id) :type m4:mat)
  (normal-matrix (m4:id) :type m4:mat))

(u:define-printer (transform-state stream :type nil)
  (format stream "TRANSFORM-STATE"))

(u:fn-> initialize-translate-state
        (transform-state &optional (or v3:vec null) (or v3:vec null))
        null)
(declaim (inline initialize-translate-state))
(defun initialize-translate-state (state &optional initial velocity)
  (declare (optimize speed))
  (when initial
    (setf (transform-state-translation/current state) (v3:copy initial)
          (transform-state-translation/previous state) (v3:copy initial)))
  (when velocity
    (setf (transform-state-translation/incremental state) (v3:copy velocity)))
  nil)

(u:fn-> initialize-rotate-state
        (transform-state &optional (or q:quat null) (or v3:vec null))
        null)
(declaim (inline initialize-rotate-state))
(defun initialize-rotate-state (state &optional initial velocity)
  (declare (optimize speed))
  (when initial
    (setf (transform-state-rotation/current state) (q:copy initial)
          (transform-state-rotation/previous state) (q:copy initial)))
  (when velocity
    (setf (transform-state-rotation/incremental state) (v3:copy velocity)))
  nil)

(u:fn-> initialize-scale-state
        (transform-state &optional (or v3:vec real null) (or v3:vec null))
        null)
(declaim (inline initialize-scale-state))
(defun initialize-scale-state (state &optional initial velocity)
  (declare (optimize speed))
  (when initial
    (let ((initial (etypecase initial
                     (v3:vec (v3:copy initial))
                     (real (v3:uniform initial)))))
      (setf (transform-state-scale/current state) initial
            (transform-state-scale/previous state) (v3:copy initial))))
  (when velocity
    (setf (transform-state-scale/incremental state) (v3:copy velocity)))
  nil)

(defun make-transform-state (&key translate translate/velocity rotate rotate/velocity scale
                               scale/velocity)
  (declare (optimize speed))
  (let ((state (%make-transform-state)))
    (initialize-translate-state state translate translate/velocity)
    (initialize-rotate-state state rotate rotate/velocity)
    (initialize-scale-state state scale scale/velocity)
    state))

(u:fn-> transform-translate-state (transform-state u:f32) null)
(declaim (inline transform-translate-state))
(defun transform-translate-state (state delta)
  (declare (optimize speed))
  (let ((current (transform-state-translation/current state))
        (incremental-delta (transform-state-translation/incremental-delta state)))
    (v3:copy! (transform-state-translation/previous state) current)
    (v3:scale! incremental-delta (transform-state-translation/incremental state) delta)
    (v3:+! current current incremental-delta)
    nil))

(u:fn-> transform-rotate-state (transform-state u:f32) null)
(declaim (inline transform-rotate-state))
(defun transform-rotate-state (state delta)
  (declare (optimize speed))
  (let ((current (transform-state-rotation/current state))
        (incremental-delta (transform-state-rotation/incremental-delta state)))
    (q:copy! (transform-state-rotation/previous state) current)
    (q:from-velocity! incremental-delta (transform-state-rotation/incremental state) delta)
    (q:rotate! current current incremental-delta)
    nil))

(u:fn-> transform-scale-state (transform-state u:f32) null)
(declaim (inline transform-scale-state))
(defun transform-scale-state (state delta)
  (declare (optimize speed))
  (let ((current (transform-state-scale/current state))
        (incremental-delta (transform-state-scale/incremental-delta state)))
    (v3:copy! (transform-state-scale/previous state) current)
    (v3:scale! incremental-delta (transform-state-scale/incremental state) delta)
    (v3:+! current current incremental-delta)
    nil))
