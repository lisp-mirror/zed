(in-package #:cl-user)

(defpackage #:%zed.framebuffer
  ;; Third-party aliases
  (:local-nicknames
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:fb.data #:%zed.framebuffer.data)
   (#:tex #:%zed.texture)
   (#:win #:%zed.window))
  (:use #:cl)
  (:shadow
   #:find
   #:load))

(in-package #:%zed.framebuffer)

(deftype target () '(member :framebuffer :read-framebuffer :draw-framebuffer))

(defstruct (framebuffer
            (:constructor %make-framebuffer)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (data nil :type fb.data::data)
  (id 0 :type u:ub16)
  (target :framebuffer :type target)
  (attachments (u:dict #'eq) :type hash-table))

(u:define-printer (framebuffer stream :type nil)
  (format stream "FRAMEBUFFER: ~s" (fb.data::name (data framebuffer))))

(u:fn-> mode->target (keyword) keyword)
(declaim (inline mode->target))
(defun mode->target (mode)
  (declare (optimize speed))
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(u:fn-> find (ctx::context symbol) (or framebuffer null))
(declaim (inline find))
(defun find (context name)
  (declare (optimize speed))
  (u:href (ctx::framebuffers context) name))

(u:fn-> point->gl (list) keyword)
(declaim (inline point->gl))
(defun point->gl (point)
  (declare (optimize speed))
  (destructuring-bind (type &optional (index 0)) point
    (ecase type
      (:color (values (u:format-symbol :keyword "~a-ATTACHMENT~d" type index)))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(u:fn-> point->render-buffer-format (list) keyword)
(declaim (inline point->render-buffer-format))
(defun point->render-buffer-format (point)
  (declare (optimize speed))
  (ecase (car point)
    (:color :rgb)
    (:depth :depth-component)
    (:stencil :stencil-index)
    (:depth/stencil :depth24-stencil8)))

(u:fn-> attachment-names->points (framebuffer list) list)
(defun attachment-names->points (framebuffer attachment-names)
  (declare (optimize speed))
  (mapcar
   (lambda (x)
     (let* ((data (data framebuffer))
            (attachment (fb.data::find-attachment data x)))
       (unless attachment
         (error "Attachment name ~s does not exist for framebuffer ~s." x (fb.data::name data)))
       (point->gl (fb.data::attachment-point attachment))))
   attachment-names))

(u:fn-> check-completeness (framebuffer target u:ub32 list) null)
(defun check-completeness (framebuffer target buffer attachment-point)
  (declare (optimize speed))
  (let ((result (gl:check-framebuffer-status target)))
    (unless (cl:find result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Framebuffer not complete: ~a~%~%Framebuffer: ~s~%Buffer: ~a~%Attachment point: ~s"
             result
             (fb.data::name (data framebuffer))
             buffer
             attachment-point))
    nil))

(u:fn-> attach-render-buffer (ctx::context framebuffer fb.data::attachment) u:ub32)
(declaim (inline attach-render-buffer))
(defun attach-render-buffer (context framebuffer attachment)
  (declare (optimize speed))
  (let* ((point (fb.data::attachment-point attachment))
         (gl-point (point->gl point))
         (format (point->render-buffer-format point))
         (buffer-id (gl:gen-renderbuffer))
         (window (ctx::window context))
         (width (funcall (fb.data::attachment-width attachment) window))
         (height (funcall (fb.data::attachment-height attachment) window))
         (target (target framebuffer)))
    (gl:bind-renderbuffer :renderbuffer buffer-id)
    (gl:renderbuffer-storage :renderbuffer format width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:bind-framebuffer target (id framebuffer))
    (gl:framebuffer-renderbuffer target gl-point :renderbuffer buffer-id)
    (check-completeness framebuffer target buffer-id point)
    (gl:bind-framebuffer target 0)
    (setf (u:href (attachments framebuffer) point) buffer-id)
    buffer-id))

(u:fn-> attach-texture (ctx::context framebuffer fb.data::attachment) u:ub32)
(declaim (inline attach-texture))
(defun attach-texture (context framebuffer attachment)
  (declare (optimize speed))
  (let ((attachment-name (fb.data::attachment-name attachment))
        (point (fb.data::attachment-point attachment))
        (buffer (fb.data::attachment-buffer attachment))
        (width-func (fb.data::attachment-width attachment))
        (height-func (fb.data::attachment-height attachment)))
    (destructuring-bind (type &optional texture-name) buffer
      (declare (ignore type))
      (unless texture-name
        (error "Framebuffer ~s uses a texture buffer without a texture name.~%~%Attachment: ~s"
               (fb.data::name (data framebuffer))
               attachment-name))
      (let* ((window (ctx::window context))
             (width (funcall width-func window))
             (height (funcall height-func window))
             (buffer-id (tex::id (tex::load context texture-name :width width :height height)))
             (gl-point (point->gl point))
             (target (target framebuffer)))
        (gl:bind-framebuffer target (id framebuffer))
        (%gl:framebuffer-texture target gl-point buffer-id 0)
        (check-completeness framebuffer target buffer-id point)
        (gl:bind-framebuffer target 0)
        (setf (u:href (attachments framebuffer) point) buffer-id)
        buffer-id))))

(u:fn-> attach (ctx::context framebuffer symbol) u:ub32)
(defun attach (context framebuffer attachment-name)
  (declare (optimize speed))
  (let ((attachment (fb.data::find-attachment (data framebuffer) attachment-name)))
    (ecase (car (fb.data::attachment-buffer attachment))
      (:render-buffer (attach-render-buffer context framebuffer attachment))
      (:texture (attach-texture context framebuffer attachment)))))

(u:fn-> attach-all (ctx::context framebuffer) null)
(defun attach-all (context framebuffer)
  (declare (optimize speed))
  (u:do-hash-values (attachment (fb.data::attachments (data framebuffer)))
    (attach context framebuffer (fb.data::attachment-name attachment))))

(u:fn-> make-framebuffer (ctx::context fb.data::data) framebuffer)
(defun make-framebuffer (context data)
  (declare (optimize speed))
  (let ((framebuffer (%make-framebuffer :data data
                                        :id (gl:gen-framebuffer)
                                        :target (mode->target (fb.data::mode data)))))
    (attach-all context framebuffer)
    (setf (u:href (ctx::framebuffers context) (fb.data::name data)) framebuffer)
    framebuffer))

(u:fn-> load (ctx::context symbol) framebuffer)
(declaim (inline load))
(defun load (context name)
  (declare (optimize speed))
  (or (find context name)
      (make-framebuffer context (fb.data::find name))))

(defmacro with-framebuffer (framebuffer (&key mode attachments) &body body)
  (u:with-gensyms (id target)
    (if framebuffer
        `(let ((,id (id ,framebuffer))
               (,target ,(if mode
                             `(mode->target ,mode)
                             `(target ,framebuffer))))
           (gl:bind-framebuffer ,target ,id)
           ,@(when attachments
               `((gl:draw-buffers ,attachments)))
           (progn ,@body)
           (gl:bind-framebuffer ,target 0))
        `(progn ,@body))))
