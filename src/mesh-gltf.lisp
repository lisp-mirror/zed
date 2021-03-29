(in-package #:cl-user)

(defpackage #:%zed.mesh.gltf
  ;; Third-party aliases
  (:local-nicknames
   (#:io #:fast-io)
   (#:sv #:static-vectors)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:asset #:%zed.asset)
   (#:bin #:%zed.binary-parser)
   (#:log #:%zed.logging)
   (#:v3 #:zed.math.vector3))
  (:use #:cl)
  (:shadow
   #:load))

(in-package #:%zed.mesh.gltf)

(u:define-constant +attribute-locations+
    '(("POSITION" . 0)
      ("NORMAL" . 1)
      ("TANGENT" . 2)
      ("COLOR_0" . 3)
      ("TEXCOORD_0" . 4)
      ("TEXCOORD_1" . 5)
      ("JOINTS_0" . 6)
      ("WEIGHTS_0" . 7))
  :test #'equal)

(defstruct (primitive
            (:constructor %make-primitive)
            (:predicate nil)
            (:copier nil))
  (vao 0 :type u:ub32)
  (mode :triangles :type keyword)
  (element-count 0 :type u:ub32)
  (component-type :unsigned-byte :type keyword)
  (vertex-buffers nil :type list)
  (index-buffer 0 :type u:ub32)
  (extent-min (v3:zero) :type v3:vec)
  (extent-max (v3:zero) :type v3:vec)
  (draw-func (constantly nil) :type function))

(defstruct (mesh
            (:predicate nil)
            (:copier nil))
  (name "" :type string)
  (primitives (vector) :type vector))

(defstruct (chunk
            (:predicate nil)
            (:copier nil))
  (length 0 :type u:ub32)
  (type 0 :type u:ub32)
  data)

(defstruct (header
            (:predicate nil)
            (:copier nil))
  (magic "" :type string)
  (version 0 :type u:ub32)
  (format-length 0 :type u:ub32))

(defstruct (datastream
            (:predicate nil)
            (:copier nil))
  (header (make-header) :type header)
  (chunks nil :type list))

(defstruct (gltf
            (:predicate nil)
            (:copier nil))
  (name "" :type string)
  (stream-length nil :type u:ub32)
  (buffer (io:make-input-buffer) :type io:input-buffer)
  (parse-tree (make-datastream) :type datastream)
  (json nil :type list)
  (buffers (vector) :type (or vector null))
  (allocated-views nil :type list)
  (meshes (u:dict #'equalp) :type hash-table))

(defun get-property (gltf key &optional object)
  (let* ((json (gltf-json gltf))
         (object (or object json)))
    (when (jsown:keyp object key)
      (jsown:val object key))))

(defun get-chunk-type (chunk)
  (case (chunk-type chunk)
    (#x4e4f534a :json-content)
    (#x004e4942 :binary-buffer)
    (otherwise :unknown)))

(defun parse-header (gltf)
  (let* ((header (make-header))
         (buffer (io:make-input-buffer :vector (bin::parse-bytes (gltf-buffer gltf) 12)))
         (magic (bin::parse-string buffer :byte-count 4)))
    (if (string= magic "glTF")
        (setf (header-magic header) magic
              (header-version header) (bin::parse-uint/le buffer 4)
              (header-format-length header) (bin::parse-uint/le buffer 4))
        (error "Invalid glTF2 file."))
    header))

(defgeneric parse-chunk-data (gltf chunk-type chunk &key)
  (:method :around (gltf chunk-type chunk &key)
    (let* ((data (bin::parse-bytes (gltf-buffer gltf) (chunk-length chunk)))
           (buffer (io:make-input-buffer :vector data)))
      (call-next-method gltf chunk-type chunk :buffer buffer))))

(defmethod parse-chunk-data (gltf (chunk-type (eql :json-content)) chunk &key buffer)
  (let ((data (bin::parse-string buffer :encoding :utf-8)))
    (setf (gltf-json gltf) (jsown:parse data))
    data))

(defmethod parse-chunk-data (gltf (chunk-type (eql :binary-buffer)) chunk &key buffer)
  (loop :with buffers = (get-property gltf "buffers")
        :with data = (make-array (length buffers))
        :for data-buffer :in buffers
        :for index :below (length buffers)
        :for size = (get-property gltf "byteLength" data-buffer)
        :do (setf (aref data index) (bin::parse-bytes buffer size))
        :finally (setf (gltf-buffers gltf) data))
  nil)

(defmethod parse-gltf-chunk-data (gltf (chunk-type (eql :unknown)) chunk &key buffer)
  (declare (ignore buffer))
  (warn "Ignoring an unknown glTF chunk type."))

(defun parse-chunk (gltf)
  (let* ((buffer (gltf-buffer gltf))
         (chunk (make-chunk :length (bin::parse-uint/le buffer 4)
                            :type (bin::parse-uint/le buffer 4)))
         (data (parse-chunk-data gltf (get-chunk-type chunk) chunk)))
    (setf (chunk-data chunk) data)))

(defun parse-chunks (gltf)
  (loop :with buffer = (gltf-buffer gltf)
        :with stream = (io:input-buffer-stream buffer)
        :until (= (io:buffer-position buffer) (gltf-stream-length gltf))
        :collect (parse-chunk gltf)))

(defun parse-datastream (gltf)
  (make-datastream :header (parse-header gltf) :chunks (parse-chunks gltf)))

(defun get-primitive-mode (gltf primitive)
  (case (get-property gltf "mode" primitive)
    (0 :points)
    (1 :lines)
    (2 :line-loop)
    (3 :line-strip)
    (4 :triangles)
    (5 :triangle-strip)
    (6 :triangle-fan)
    (otherwise :triangles)))

(defun make-buffer (gltf target accessor)
  (let ((buffer-view-index (get-property gltf "bufferView" accessor)))
    (unless (find buffer-view-index (gltf-allocated-views gltf))
      (let* ((buffer-view (elt (get-property gltf "bufferViews") buffer-view-index))
             (index (get-property gltf "buffer" buffer-view))
             (offset (+ (or (get-property gltf "byteOffset" accessor) 0)
                        (or (get-property gltf "byteOffset" buffer-view) 0)))
             (size (get-property gltf "byteLength" buffer-view))
             (buffer (aref (gltf-buffers gltf) index))
             (data (sv:make-static-vector size
                                          :element-type 'u:octet
                                          :initial-contents (subseq buffer offset (+ offset size))))
             (pointer (sv:static-vector-pointer data))
             (buffer-id (gl:gen-buffer)))
        (gl:bind-buffer target buffer-id)
        (%gl:buffer-data target size pointer :static-draw)
        (sv:free-static-vector data)
        (push buffer-view-index (gltf-allocated-views gltf))
        buffer-id))))

(defun get-component-type (gltf accessor)
  (ecase (get-property gltf "componentType" accessor)
    (5120 :byte)
    (5121 :unsigned-byte)
    (5122 :short)
    (5123 :unsigned-short)
    (5125 :unsigned-int)
    (5126 :float)))

(defun get-component-count (data-type)
  (ecase (u:make-keyword data-type)
    (:scalar 1)
    (:vec2 2)
    (:vec3 3)
    ((:vec4 :mat2) 4)
    (:mat3 9)
    (:mat4 16)))

(defun get-attribute-location (name)
  (u:alist-get +attribute-locations+ name :test #'string=))

(defun get-attribute-normalization (name component-type)
  (if (and (or (eq component-type :unsigned-byte)
               (eq component-type :unsigned-short))
           (not (string= name "JOINTS_0")))
      :true
      :false))

(defun configure-attribute (gltf attribute accessor)
  (let* ((buffer-view (elt (get-property gltf "bufferViews")
                           (get-property gltf "bufferView" accessor)))
         (type (get-component-type gltf accessor))
         (count (get-component-count (get-property gltf "type" accessor)))
         (stride (or (get-property gltf "byteStride" buffer-view) 0))
         (location (get-attribute-location attribute))
         (normalize (get-attribute-normalization attribute type)))
    (gl:enable-vertex-attrib-array location)
    (%gl:vertex-attrib-pointer location count type normalize stride 0)))

(defun make-vertex-buffers (gltf primitive data)
  (jsown:do-json-keys (attr accessor-id) (get-property gltf "attributes" data)
    (let* ((accessor (elt (get-property gltf "accessors") accessor-id))
           (count (get-property gltf "count" accessor))
           (buffer (make-buffer gltf :array-buffer accessor)))
      (push buffer (primitive-vertex-buffers primitive))
      (configure-attribute gltf attr accessor)
      (when (string= attr "POSITION")
        (let ((min-extent (get-property gltf "min" accessor))
              (max-extent (get-property gltf "max" accessor)))
          (destructuring-bind (min-x min-y min-z) min-extent
            (destructuring-bind (max-x max-y max-z) max-extent
              (setf (primitive-element-count primitive) count
                    (primitive-extent-min primitive) (v3:vec (float min-x 1f0)
                                                             (float min-y 1f0)
                                                             (float min-z 1f0))
                    (primitive-extent-max primitive) (v3:vec (float max-x 1f0)
                                                             (float max-y 1f0)
                                                             (float max-z 1f0))))))))))

(defun make-index-buffer (gltf primitive data)
  (u:when-let* ((indices (get-property gltf "indices" data))
                (accessor (elt (get-property gltf "accessors") indices))
                (element-count (get-property gltf "count" accessor))
                (component-type (get-component-type gltf accessor))
                (index-buffer (make-buffer gltf :element-array-buffer accessor)))
    (setf (primitive-element-count primitive) element-count
          (primitive-component-type primitive) component-type
          (primitive-index-buffer primitive) index-buffer)))

(defun draw-primitive/indexed (primitive instance-count)
  (declare (optimize speed))
  (gl:bind-vertex-array (primitive-vao primitive))
  (gl:bind-buffer :element-array-buffer (primitive-index-buffer primitive))
  (%gl:draw-elements-instanced (primitive-mode primitive)
                               (primitive-element-count primitive)
                               (primitive-component-type primitive)
                               0
                               instance-count))

(defun draw-primitive/vertices (primitive instance-count)
  (declare (optimize speed))
  (gl:bind-vertex-array (primitive-vao primitive))
  (gl:draw-arrays-instanced (primitive-mode primitive)
                            0
                            (primitive-element-count primitive)
                            instance-count))

(defun make-draw-func (primitive)
  (setf (primitive-draw-func primitive)
        (if (primitive-index-buffer primitive)
            (lambda (x) (draw-primitive/indexed primitive x))
            (lambda (x) (draw-primitive/vertices primitive x)))))

(defun make-primitive (gltf mesh-name data)
  (let* ((vao (gl:gen-vertex-array))
         (primitive (%make-primitive :vao vao :mode (get-primitive-mode gltf data))))
    (gl:bind-vertex-array vao)
    (make-vertex-buffers gltf primitive data)
    (make-index-buffer gltf primitive data)
    (make-draw-func primitive)
    (log::debug :zed.mesh.gltf "Loaded mesh: ~a, primitive: ~a (VAO: ~d)"
                (gltf-name gltf)
                mesh-name
                vao)
    primitive))

(defun parse-meshes (gltf)
  (loop :for mesh :in (get-property gltf "meshes")
        :for index :from 0
        :for name = (or (get-property gltf "name" mesh)
                        (format nil "~a~d" (gltf-name gltf) index))
        :for primitives = (map
                           'vector
                           (lambda (x)
                             (make-primitive gltf name x))
                           (get-property gltf "primitives" mesh))
        :do (setf (u:href (gltf-meshes gltf) name)
                  (make-mesh :name name :primitives primitives))))

(defun find-mesh (gltf index)
  (let ((meshes (get-property gltf "meshes")))
    (when (>= index (length meshes))
      (error "Mesh index ~d not found." index))
    (get-property gltf "primitives" (elt meshes index))))

(defun load (asset)
  (asset::with-asset (asset path data :length-binding length)
    (let* ((buffer (io:make-input-buffer :stream data))
           (gltf (make-gltf :name (pathname-name path) :buffer buffer :stream-length length)))
      (setf (gltf-parse-tree gltf) (parse-datastream gltf))
      (parse-meshes gltf)
      (setf (gltf-buffers gltf) nil)
      gltf)))
