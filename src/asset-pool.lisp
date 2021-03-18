(in-package #:cl-user)

(defpackage #:%zed.asset-pool
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context))
  (:use #:cl)
  (:shadow
   #:delete
   #:find))

(in-package #:%zed.asset-pool)

(glob:define-global-var =pools= (u:dict #'eq))

(defstruct (pool
            (:constructor %make-pool)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (system nil :type symbol)
  (path #p"" :type pathname)
  (assets (u:dict #'eq) :type hash-table))

(u:define-printer (pool stream :type nil)
  (format stream "ASSET-POOL: ~s" (pool-name pool)))

(defstruct (asset-data
            (:constructor %make-asset-data)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (pool nil :type symbol)
  (name nil :type symbol)
  (path #p"" :type pathname))

(u:define-printer (asset-data stream :type nil)
  (format stream "ASSET: ~s (pool: ~s)"
          (name asset-data)
          (pool asset-data)))

(defun find-pool (name)
  (u:href =pools= name))

(defun make-asset-data (pool-name base-path data)
  (destructuring-bind (name path) data
    (let* ((pool (find-pool pool-name))
           (base-path (uiop:ensure-directory-pathname base-path))
           (path (uiop:merge-pathnames* path base-path))
           (asset (%make-asset-data :pool pool-name :name name :path path)))
      (setf (u:href (pool-assets pool) name) asset)
      asset)))

(defun find-asset-data (pool-name name)
  (u:if-let ((pool (find-pool pool-name)))
    (or (u:href (pool-assets pool) name)
        (error "Asset ~s not found in pool ~s." name pool-name))
    (error "Asset pool ~s does not exist." pool-name)))

(defun make-asset-symbol (path)
  (intern (string-upcase (cl-slug:slugify (pathname-name path)))))

(defun collect-path-p (path filter)
  (flet ((normalize-type (type)
           (string-downcase (string-left-trim "." type))))
    (let ((path-type (string-downcase (pathname-type path))))
      (some (lambda (x) (string= path-type (normalize-type x)))
            (u:ensure-list filter)))))

(defun %resolve-path (system path)
  #+(and zed.release sbcl)
  (uiop:merge-pathnames* path (uiop:pathname-directory-pathname (first sb-ext:*posix-argv*)))
  #+(and zed.release (not sbcl))
  (error "Releases must be deployed with SBCL to load assets.")
  #-zed.release
  (asdf:system-relative-pathname system path))

(defun update-pool (pool-name system path filter)
  (let* ((pool (find-pool pool-name))
         (path (uiop:ensure-directory-pathname path))
         (resolved-path (%resolve-path system path)))
    (setf (pool-system pool) system
          (pool-path pool) path)
    (clrhash (pool-assets pool))
    (u:map-files
     resolved-path
     (lambda (x)
       (let* ((asset-name (make-asset-symbol x))
              (file-name (file-namestring x))
              (data (list asset-name file-name)))
         (u:if-found (existing (u:href (pool-assets pool) asset-name))
           (error "Asset pool ~s has ambiguously named assets.~%~%~
                   File 1: ~a~%File 2: ~a~%Normalized name: ~a"
                  pool-name
                  file-name
                  (file-namestring (path existing))
                  asset-name)
           (make-asset-data pool-name path data))))
     :test (lambda (x) (if filter (collect-path-p x filter) t))
     :recursive-p nil)))

(defun make-pool (name system path filter)
  (let ((pool (%make-pool :name name)))
    (setf (u:href =pools= name) pool)
    (update-pool name system path filter)
    pool))

(defmacro define-asset-pool (name (&key system) &body body)
  (destructuring-bind (&key path filter) body
    `(progn
       (unless ,system
         (error "Asset pool must specify the name of an ASDF system."))
       (if (u:href =pools= ',name)
           (update-pool ',name ,system ,path ',filter)
           (make-pool ',name ,system ,path ',filter)))))

(defun find (context type key)
  (u:href (ctx::assets context) type key))

(defun delete (context type key)
  (remhash key (u:href (ctx::assets context) type)))

(defgeneric resolve-path (pool/asset &key))

(defmethod resolve-path ((pool-name symbol) &key)
  (let* ((pool (find-pool pool-name))
         (system (pool-system pool))
         (path (%resolve-path system (pool-path pool))))
    (ensure-directories-exist path)
    path))

(defmethod resolve-path ((pool pool) &key)
  (resolve-path (pool-name pool)))

(defmethod resolve-path ((asset list) &key (not-found-error-p t))
  (destructuring-bind (pool-name asset-name) asset
    (let* ((pool (find-pool pool-name))
           (data (find-asset-data pool-name asset-name))
           (system (pool-system pool))
           (path (%resolve-path system (path data))))
      (if not-found-error-p
          (if (uiop:file-exists-p path)
              (values path data)
              (error "File not found for asset ~s of pool ~s.~%~%Path: ~s"
                     asset-name
                     pool-name
                     path))
          (values path data)))))

(defmacro with-asset ((context type key) &body body)
  (u:with-gensyms (table value found-p)
    `(symbol-macrolet ((,table (u:href (ctx::assets ,context) ,type)))
       (u:mvlet ((,value ,found-p ,table))
         (unless ,found-p
           (setf ,table (u:dict #'equalp))))
       (u:ensure-gethash ,key ,table (progn ,@body)))))
