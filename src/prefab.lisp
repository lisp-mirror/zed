(in-package #:zed)

(defstruct (prefab-node
            (:constructor %make-prefab-node)
            (:predicate nil)
            (:copier nil))
  (prefab nil :type (or prefab null))
  (path nil :type list)
  (parent nil :type (or prefab-node null))
  (options (u:dict #'eq) :type hash-table)
  (template nil :type (or prefab-node null))
  (trait-options (u:dict #'eq) :type hash-table)
  (trait-args (u:dict #'eq) :type hash-table)
  (trait-thunked-args (u:dict #'eq) :type hash-table))

(u:define-printer (prefab-node stream :type nil)
  (format stream "PREFAB-NODE: ~{~a~^/~}" (prefab-node-path prefab-node)))

(defstruct (prefab-factory
            (:constructor %make-prefab-factory)
            (:predicate nil)
            (:copier nil))
  (prefab-name nil :type symbol)
  (current-node nil :type (or prefab-node null))
  (game-objects (u:dict #'equal) :type hash-table)
  (func (constantly nil) :type function))

(defstruct (prefab-reference
            (:constructor %make-prefab-reference)
            (:predicate nil)
            (:copier nil))
  (func (constantly nil) :type function))

(defstruct (prefab
            (:constructor %make-prefab)
            (:predicate nil)
            (:copier nil))
  (name nil :type symbol)
  (data nil :type list)
  (root nil :type (or prefab-node null))
  (nodes (u:dict #'equal) :type hash-table)
  (masters nil :type list)
  (slaves nil :type list)
  (factory (%make-prefab-factory) :type prefab-factory))

(u:define-printer (prefab stream :type nil)
  (format stream "PREFAB: ~s" (prefab-name prefab)))

(glob:define-global-var =prefabs= (u:dict #'eq))

(defun make-prefab (name data)
  (let ((prefab (%make-prefab :name name :data data)))
    (setf (u:href =prefabs= name) prefab)))

(defun reset-prefab (name data)
  (let ((prefab (u:href =prefabs= name)))
    (setf (prefab-data prefab) data
          (prefab-root prefab) nil)
    (clrhash (prefab-nodes prefab))))

(defun prefab-path-mergeable-p (path1 path2)
  (and (not (equal path1 path2))
       (equal path2 (subseq path1 0 (min (length path1) (length path2))))))

(defun copy-prefab-template (template target-prefab target-path)
  (let* ((nodes (prefab-nodes target-prefab))
         (args (prefab-node-trait-args template))
         (thunked-args (prefab-node-trait-thunked-args template))
         (node (%make-prefab-node :prefab target-prefab
                                  :path target-path
                                  :parent (u:href nodes (butlast target-path))
                                  :template template
                                  :trait-args args
                                  :trait-thunked-args thunked-args)))
    (setf (u:href nodes target-path) node)))

(defun process-prefab-trait-data (trait)
  (destructuring-bind (type . options/args) trait
    (let* ((options-p (listp (first options/args)))
           (args (if options-p (rest options/args) options/args))
           (thunked-args (loop :for (k v) :on args :by #'cddr
                               :nconc `(,k '(lambda () ,v)))))
      `(list ',type
             ,(when options-p `(,@(first options/args)))
             ,@thunked-args))))

(defun process-prefab-node-data (data)
  (flet ((split-traits (data)
           (loop :for cons :on data
                 :for form = (car cons)
                 :while (symbolp (car form))
                 :collect form :into traits
                 :finally (return (values traits cons)))))
    (destructuring-bind ((name . options) . rest) data
      (u:mvlet ((traits children (split-traits rest)))
        `(list ',name
               ,(when options
                  `(list ,@options))
               ,(when traits
                  `(list ,@(mapcar #'process-prefab-trait-data traits)))
               ,(when children
                  `(list ,@(mapcar #'process-prefab-node-data children))))))))

(defun preprocess-prefab-data (name options data)
  (let ((data `(((,name ,@options) ,@data))))
    `(list ,@(mapcar #'process-prefab-node-data data))))

(defun parse-prefab-trait-args (data)
  (flet ((%parse (data func)
           (let ((seen-types nil))
             (apply #'u:dict
                    (mapcan
                     (lambda (x)
                       (destructuring-bind (type options . args) x
                         (declare (ignore options))
                         (when (member type seen-types)
                           (error "A prefab node cannot have multiple traits of the same type.~%~%~
                                   Duplicate trait type: ~s"
                                  type))
                         (prog1 (list type
                                      (u:plist->hash
                                       (loop :for (k v) :on args :by #'cddr
                                             :collect k
                                             :collect (funcall func v))
                                       :test #'eq))
                           (push type seen-types))))
                     data)))))
    (values (%parse data (lambda (x) (nth 2 x)))
            (%parse data (lambda (x) (compile nil x))))))

(defun parse-prefab-trait-options (data)
  (apply #'u:dict
         (mapcan
          (lambda (x)
            (destructuring-bind (type options . args) x
              (declare (ignore args))
              (list type options)))
          data)))

(defun parse-prefab-node-template (data path)
  (let* ((template-path (u:ensure-list (getf data :template)))
         (template-root-path (first template-path))
         (prefab (u:href =prefabs= template-root-path)))
    (when template-path
      (unless (u:href =prefabs= template-root-path)
        (error "Template ~s does not exist for prefab node ~{~a~^/~}." template-path path))
      (when (eq template-root-path (first path))
        (error "Cannot have a template to a node within the same prefab."))
      (or (u:href (prefab-nodes prefab) template-path)
          (error "Template node ~{~a~^/~} not found while processing node: ~{~a~^/~}.~%~
                  This may mean that you just changed the tree structure of a prefab that another ~
                  templated node is trying to reference."
                 template-path
                 path)))))

(defun parse-prefab-nodes (prefab data &optional parent)
  (destructuring-bind (name options traits children) data
    (u:mvlet* ((nodes (prefab-nodes prefab))
               (path `(,@parent ,name))
               (parent (u:href nodes (butlast path)))
               (args thunked-args (parse-prefab-trait-args traits))
               (trait-options (parse-prefab-trait-options traits))
               (template (parse-prefab-node-template options path))
               (node (%make-prefab-node :prefab prefab
                                        :path path
                                        :parent parent
                                        :options (apply #'u:dict #'eq options)
                                        :template template
                                        :trait-options trait-options
                                        :trait-args args
                                        :trait-thunked-args thunked-args)))
      (setf (u:href nodes path) node)
      (map nil (lambda (x) (parse-prefab-nodes prefab x path)) children))))

(defun populate-explicit-prefab-nodes (prefab)
  (let ((name (prefab-name prefab))
        (data (prefab-data prefab))
        (nodes (prefab-nodes prefab)))
    (map nil (lambda (x) (parse-prefab-nodes prefab x)) data)
    (setf (prefab-root prefab) (u:href nodes (list name)))))

(defun populate-implicit-prefab-nodes (prefab)
  (let ((nodes (prefab-nodes prefab)))
    (flet ((populate (parent template)
             (let ((template-path (prefab-node-path template)))
               (u:do-hash (path node (prefab-nodes (u:href =prefabs= (car template-path))))
                 (let ((target-path (append (prefab-node-path parent)
                                            (nthcdr (length template-path) path))))
                   (u:if-let ((target (u:href nodes target-path)))
                     (setf (prefab-node-template target) node)
                     (copy-prefab-template node prefab target-path)))))))
      (u:do-hash-values (node (u:copy-hash-table nodes))
        (u:when-let ((template (prefab-node-template node)))
          (populate node template))))))

(defun record-prefab-dependencies (prefab)
  (let ((old-masters (prefab-masters prefab)))
    (setf (prefab-masters prefab) nil)
    (u:do-hash-values (node (prefab-nodes prefab))
      (u:when-let* ((template (prefab-node-template node))
                    (template-prefab (prefab-node-prefab template)))
        (pushnew (prefab-name prefab) (prefab-slaves template-prefab))
        (pushnew (prefab-name template-prefab) (prefab-masters prefab))))
    (dolist (master-spec old-masters)
      (let ((master (u:href =prefabs= master-spec)))
        (unless (find master-spec (prefab-masters prefab))
          (u:deletef (prefab-slaves master) (prefab-name prefab)))))))

(defun merge-prefab-trait-args (node node-args template-args policy)
  (u:do-hash (k v template-args)
    (u:if-let ((args (u:href node-args k)))
      (setf args (u:hash-merge v args))
      (setf (u:href node-args k) (u:copy-hash-table v))))
  (u:do-hash (k v node-args)
    (let* ((options (u:href (prefab-node-trait-options node) k))
           (policy (or (getf options :policy) policy)))
      (u:when-let ((source (u:href template-args k)))
        (when (eq policy :overlay)
          (u:do-hash (k new-arg (u:hash-merge source v))
            (setf (u:href v k) new-arg)))))))

(defun merge-prefab-options (prefab)
  (u:do-hash-values (node (prefab-nodes prefab))
    (u:when-let ((template (prefab-node-template node)))
      (let ((options1 (prefab-node-options node))
            (options2 (prefab-node-options template)))
        (setf (prefab-node-options node) (u:hash-merge options2 options1))))))

(defun merge-prefab-traits (prefab)
  (u:do-hash-values (node (prefab-nodes prefab))
    (u:when-let ((template (prefab-node-template node)))
      (let ((args1 (prefab-node-trait-args node))
            (args2 (prefab-node-trait-args template))
            (thunks1 (prefab-node-trait-thunked-args node))
            (thunks2 (prefab-node-trait-thunked-args template))
            (policy (or (u:href (prefab-node-options node) :policy) :overlay)))
        (merge-prefab-trait-args node args1 args2 policy)
        (merge-prefab-trait-args node thunks1 thunks2 policy)))))

(defun parse-prefab (prefab)
  (let (success-p)
    (unwind-protect
         (progn
           (populate-explicit-prefab-nodes prefab)
           (populate-implicit-prefab-nodes prefab)
           (record-prefab-dependencies prefab)
           (merge-prefab-options prefab)
           (merge-prefab-traits prefab)
           (build-prefab prefab)
           (setf success-p t))
      (unless success-p
        (remhash (prefab-name prefab) =prefabs=)))))

(defun update-prefab (prefab)
  (parse-prefab prefab)
  (when =core=
    (thread-pool-enqueue (list :prefab (prefab-name prefab))))
  (dolist (spec (prefab-slaves prefab))
    (u:when-let ((slave (u:href =prefabs= spec)))
      (clrhash (prefab-nodes slave))
      (update-prefab slave))))

(defun load-prefab (core name &key parent (viewport :default))
  (let ((prefabs =prefabs=))
    (u:if-let ((prefab (u:href prefabs name)))
      (let* ((factory (prefab-factory prefab))
             (game-object (funcall (prefab-factory-func factory)
                                   core
                                   :parent parent
                                   :viewport viewport)))
        (clrhash (prefab-factory-game-objects factory))
        game-object)
      (error "Prefab ~s not defined." name))))

(defun find-prefab-reference-path (factory path)
  (let ((game-objects (prefab-factory-game-objects factory))
        (current-path (prefab-node-path (prefab-factory-current-node factory))))
    (u:if-let ((index (search path current-path)))
      (u:href game-objects (subseq current-path 0 (+ index (length path))))
      (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}." path current-path))))

(defun find-prefab-reference-down (factory path &optional current)
  (let ((current-path (or current (prefab-node-path (prefab-factory-current-node factory)))))
    (or (u:href (prefab-factory-game-objects factory) (append current-path path))
        (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}." path current-path))))

(defun find-prefab-reference-up (factory level path)
  (let* ((current-path (prefab-node-path (prefab-factory-current-node factory)))
         (parent-path (butlast current-path level)))
    (if path
        (find-prefab-reference-down factory path parent-path)
        (or (u:href (prefab-factory-game-objects factory) parent-path)
            (error "Failed to find parent reference ~{~a~^/~}." parent-path)))))

(defun generate-prefab-reference-function (reference-spec)
  (lambda (factory)
    (destructuring-bind (type . args) reference-spec
      (case type
        (:up
         (destructuring-bind (level &rest path) args
           (find-prefab-reference-up factory level path)))
        (:down
         (find-prefab-reference-down factory args))
        (:path
         (find-prefab-reference-path factory args))
        (t
         (error "Reference spec type must be :UP, :DOWN, or :PATH."))))))

(defmacro prefab-reference (&rest reference-spec)
  `(%make-prefab-reference :func (generate-prefab-reference-function ',reference-spec)))

(defmacro define-prefab (name options &body body)
  (u:with-gensyms (data)
    (u:mvlet ((body decls doc (u:parse-body body :documentation t)))
      `(let ((,data ,(preprocess-prefab-data name options body)))
         (if (u:href =prefabs= ',name)
             (reset-prefab ',name ,data)
             (make-prefab ',name ,data))
         (update-prefab (u:href =prefabs= ',name))))))

(defmethod recompile ((type (eql :prefab)) data)
  (with-scope (:prefab-recompile)
    (dolist (game-object (u:href (core-prefabs =core=) data))
      (destroy-game-object =core= game-object)
      (let* ((parent (game-object-parent game-object))
             (translation (get-translation game-object))
             (new-game-object (load-prefab =core= data :parent parent)))
        (translate new-game-object translation :replace-p t)))
    (v:debug :zed "Recompiled prefab: ~s" data)))

(defun realize-prefab-node (core node root viewport)
  (let* ((factory (prefab-factory (prefab-node-prefab node)))
         (game-objects (prefab-factory-game-objects factory))
         (game-object (u:href game-objects (prefab-node-path node))))
    (spawn-game-object core
                       game-object
                       :parent (u:if-let ((parent (prefab-node-parent node)))
                                 (u:href game-objects (prefab-node-path parent))
                                 root)
                       :viewport viewport)
    (u:do-hash (type args (prefab-node-trait-thunked-args node))
      (loop :for (k v) :on (u:hash->plist args) :by #'cddr
            :collect k :into args
            :collect (let ((arg (funcall v)))
                       (if (typep arg 'prefab-reference)
                           (funcall (prefab-reference-func arg) factory)
                           arg))
              :into args
            :finally (let ((trait (apply #'make-trait core type args)))
                       (attach-trait game-object trait))))
    game-object))

(defun register-prefab-root (core prefab)
  (let* ((factory (prefab-factory prefab))
         (root (u:href (prefab-factory-game-objects factory)
                       (prefab-node-path (prefab-root prefab))))
         (prefab-name (prefab-name prefab)))
    (push root (u:href (core-prefabs core) prefab-name))
    (setf (game-object-prefab-name root) prefab-name)
    root))

(defun initialize-prefab-node-transforms (game-object node)
  (let ((node-options (prefab-node-options node))
        (transform (game-object-transform game-object)))
    (initialize-translate-state transform
                                (u:href node-options :translate)
                                (u:href node-options :translate-velocity))
    (initialize-rotate-state transform
                             (u:href node-options :rotate)
                             (u:href node-options :rotate-velocity))
    (initialize-scale-state transform
                            (u:href node-options :scale)
                            (u:href node-options :scale-velocity))))

(defun make-prefab-factory-function (prefab)
  (lambda (core &key parent (viewport :default))
    (let ((factory (prefab-factory prefab))
          (nodes (prefab-nodes prefab)))
      (u:do-hash (path node nodes)
        (let* ((label (format nil "~(~a~)" (first (last path))))
               (game-object (make-game-object :label label)))
          (initialize-prefab-node-transforms game-object node)
          (setf (u:href (prefab-factory-game-objects factory) path) game-object)))
      (u:do-hash-values (node nodes)
        (setf (prefab-factory-current-node factory) node)
        (with-scope (:prefab-instantiate)
          (realize-prefab-node core node parent viewport)))
      (setf (prefab-factory-current-node factory) nil)
      (register-prefab-root core prefab))))

(defun build-prefab (prefab)
  (let ((factory (prefab-factory prefab)))
    (setf (prefab-factory-func factory) (make-prefab-factory-function prefab))))
