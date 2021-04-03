(in-package #:zed)

(deftype framebuffer-target () '(member :framebuffer :read-framebuffer :draw-framebuffer))

(defstruct (framebuffer
            (:constructor %make-framebuffer)
            (:predicate nil)
            (:copier nil))
  (data nil :type framebuffer-data)
  (id 0 :type u:ub16)
  (target :framebuffer :type framebuffer-target)
  (attachments (u:dict #'eq) :type hash-table))

(u:define-printer (framebuffer stream :type nil)
  (format stream "FRAMEBUFFER: ~s" (framebuffer-data-name (framebuffer-data framebuffer))))

(u:fn-> framebuffer-mode->target (keyword) keyword)
(declaim (inline framebuffer-mode->target))
(defun framebuffer-mode->target (mode)
  (declare (optimize speed))
  (ecase mode
    (:read :read-framebuffer)
    (:write :draw-framebuffer)
    (:read/write :framebuffer)))

(u:fn-> find-framebuffer (context symbol) (or framebuffer null))
(declaim (inline find-framebuffer))
(defun find-framebuffer (context name)
  (declare (optimize speed))
  (u:href (context-framebuffers context) name))

(u:fn-> framebuffer-point->gl (list) keyword)
(declaim (inline framebuffer-point->gl))
(defun framebuffer-point->gl (point)
  (declare (optimize speed))
  (destructuring-bind (type &optional (index 0)) point
    (ecase type
      (:color (values (u:format-symbol :keyword "~a-ATTACHMENT~d" type index)))
      (:depth :depth-attachment)
      (:stencil :stencil-attachment)
      (:depth/stencil :depth-stencil-attachment))))

(u:fn-> framebuffer-point->render-buffer-format (list) keyword)
(declaim (inline framebuffer-point->render-buffer-format))
(defun framebuffer-point->render-buffer-format (point)
  (declare (optimize speed))
  (ecase (car point)
    (:color :rgb)
    (:depth :depth-component)
    (:stencil :stencil-index)
    (:depth/stencil :depth24-stencil8)))

(u:fn-> framebuffer-attachment-names->points (framebuffer list) list)
(defun framebuffer-attachment-names->points (framebuffer attachment-names)
  (declare (optimize speed))
  (mapcar
   (lambda (x)
     (let* ((data (framebuffer-data framebuffer))
            (attachment (find-framebuffer-attachment data x)))
       (unless attachment
         (error "Attachment name ~s does not exist for framebuffer ~s."
                x
                (framebuffer-data-name data)))
       (framebuffer-point->gl (framebuffer-attachment-point attachment))))
   attachment-names))

(u:fn-> check-framebuffer-completeness (framebuffer framebuffer-target u:ub32 list) null)
(defun check-framebuffer-completeness (framebuffer target buffer attachment-point)
  (declare (optimize speed))
  (let ((result (gl:check-framebuffer-status target)))
    (unless (find result '(:framebuffer-complete :framebuffer-complete-oes))
      (error "Framebuffer not complete: ~a~%~%Framebuffer: ~s~%Buffer: ~a~%Attachment point: ~s"
             result
             (framebuffer-data-name (framebuffer-data framebuffer))
             buffer
             attachment-point))
    nil))

(u:fn-> framebuffer-attach-render-buffer (context framebuffer framebuffer-attachment) u:ub32)
(declaim (inline framebuffer-attach-render-buffer))
(defun framebuffer-attach-render-buffer (context framebuffer attachment)
  (declare (optimize speed))
  (let* ((point (framebuffer-attachment-point attachment))
         (gl-point (framebuffer-point->gl point))
         (format (framebuffer-point->render-buffer-format point))
         (buffer-id (gl:gen-renderbuffer))
         (window (context-window context))
         (width (funcall (framebuffer-attachment-width attachment) window))
         (height (funcall (framebuffer-attachment-height attachment) window))
         (target (framebuffer-target framebuffer)))
    (gl:bind-renderbuffer :renderbuffer buffer-id)
    (gl:renderbuffer-storage :renderbuffer format width height)
    (gl:bind-renderbuffer :renderbuffer 0)
    (gl:bind-framebuffer target (framebuffer-id framebuffer))
    (gl:framebuffer-renderbuffer target gl-point :renderbuffer buffer-id)
    (check-framebuffer-completeness framebuffer target buffer-id point)
    (gl:bind-framebuffer target 0)
    (setf (u:href (framebuffer-attachments framebuffer) point) buffer-id)
    buffer-id))

(u:fn-> framebuffer-attach-texture (context framebuffer framebuffer-attachment) u:ub32)
(declaim (inline attach-framebuffer-texture))
(defun attach-framebuffer-texture (context framebuffer attachment)
  (declare (optimize speed))
  (let ((attachment-name (framebuffer-attachment-name attachment))
        (point (framebuffer-attachment-point attachment))
        (buffer (framebuffer-attachment-buffer attachment))
        (width-func (framebuffer-attachment-width attachment))
        (height-func (framebuffer-attachment-height attachment)))
    (destructuring-bind (type &optional texture-name) buffer
      (declare (ignore type))
      (unless texture-name
        (error "Framebuffer ~s uses a texture buffer without a texture name.~%~%Attachment: ~s"
               (framebuffer-data-name (framebuffer-data framebuffer))
               attachment-name))
      (let* ((window (context-window context))
             (width (funcall width-func window))
             (height (funcall height-func window))
             (buffer-id (texture-id
                         (load-texture context texture-name :width width :height height)))
             (gl-point (framebuffer-point->gl point))
             (target (framebuffer-target framebuffer)))
        (gl:bind-framebuffer target (framebuffer-id framebuffer))
        (%gl:framebuffer-texture target gl-point buffer-id 0)
        (check-framebuffer-completeness framebuffer target buffer-id point)
        (gl:bind-framebuffer target 0)
        (setf (u:href (framebuffer-attachments framebuffer) point) buffer-id)
        buffer-id))))

(u:fn-> framebuffer-attach (context framebuffer symbol) u:ub32)
(defun framebuffer-attach (context framebuffer attachment-name)
  (declare (optimize speed))
  (let ((attachment (find-framebuffer-attachment (framebuffer-data framebuffer) attachment-name)))
    (ecase (car (framebuffer-attachment-buffer attachment))
      (:render-buffer (framebuffer-attach-render-buffer context framebuffer attachment))
      (:texture (framebuffer-attach-texture context framebuffer attachment)))))

(u:fn-> framebuffer-attach-all (context framebuffer) null)
(defun framebuffer-attach-all (context framebuffer)
  (declare (optimize speed))
  (u:do-hash-values (attachment (framebuffer-data-attachments (framebuffer-data framebuffer)))
    (framebuffer-attach context framebuffer (framebuffer-attachment-name attachment))))

(u:fn-> make-framebuffer (context framebuffer-data) framebuffer)
(defun make-framebuffer (context data)
  (declare (optimize speed))
  (let ((framebuffer (%make-framebuffer
                      :data data
                      :id (gl:gen-framebuffer)
                      :target (framebuffer-mode->target (framebuffer-data-mode data)))))
    (framebuffer-attach-all context framebuffer)
    (setf (u:href (context-framebuffers context) (framebuffer-data-name data)) framebuffer)
    framebuffer))

(u:fn-> load-framebuffer (context symbol) framebuffer)
(declaim (inline load-framebuffer))
(defun load-framebuffer (context name)
  (declare (optimize speed))
  (or (find-framebuffer context name)
      (make-framebuffer context (find-framebuffer-data name))))

(defmacro with-framebuffer (framebuffer (&key mode attachments) &body body)
  (u:with-gensyms (id target)
    (if framebuffer
        `(let ((,id (framebuffer-id ,framebuffer))
               (,target ,(if mode
                             `(framebuffer-mode->target ,mode)
                             `(framebuffer-target ,framebuffer))))
           (gl:bind-framebuffer ,target ,id)
           ,@(when attachments
               `((gl:draw-buffers ,attachments)))
           (progn ,@body)
           (gl:bind-framebuffer ,target 0))
        `(progn ,@body))))
