(in-package #:cl-user)

(defpackage #:%zed.prefab
  ;; Third-party aliases
  (:local-nicknames
   (#:glob #:global-vars)
   (#:u #:golden-utils))
  ;; Internal aliases
  (:local-nicknames
   (#:ctx #:%zed.context)
   (#:gob #:%zed.game-object)
   (#:pf.def #:%zed.prefab.definitions)
   (#:pf.fac #:%zed.prefab.factory))
  (:use #:cl))

(in-package #:%zed.prefab)

(glob:define-global-var =data= (u:dict #'eq))

(defun make-prefab (name data)
  (let ((prefab (pf.def::%make-prefab :name name :data data)))
    (setf (u:href =data= name) prefab)))

(defun reset (name data)
  (let ((prefab (u:href =data= name)))
    (setf (pf.def::data prefab) data
          (pf.def::root prefab) nil)
    (clrhash (pf.def::nodes prefab))))

(defun path-mergeable-p (path1 path2)
  (and (not (equal path1 path2))
       (equal path2 (subseq path1 0 (min (length path1) (length path2))))))

(defun copy-template (template target-prefab target-path)
  (let* ((nodes (pf.def::nodes target-prefab))
         (args (pf.def::trait-args template))
         (thunked-args (pf.def::trait-thunked-args template))
         (node (pf.def::%make-node :prefab target-prefab
                                   :path target-path
                                   :parent (u:href nodes (butlast target-path))
                                   :template template
                                   :trait-args args
                                   :trait-thunked-args thunked-args)))
    (setf (u:href nodes target-path) node)))

(defun merge-trait-args (node node-args template-args policy)
  (u:do-hash (k v template-args)
    (u:if-let ((args (u:href node-args k)))
      (setf args (u:hash-merge v args))
      (setf (u:href node-args k) (u:copy-hash-table v))))
  (u:do-hash (k v node-args)
    (let* ((options (u:href (pf.def::trait-options node) k))
           (policy (or (getf options :policy) policy)))
      (u:when-let ((source (u:href template-args k)))
        (when (eq policy :overlay)
          (u:do-hash (k new-arg (u:hash-merge source v))
            (setf (u:href v k) new-arg)))))))

(defun process-trait-data (trait)
  (destructuring-bind (type . options/args) trait
    (let* ((options-p (listp (first options/args)))
           (args (if options-p (rest options/args) options/args))
           (thunked-args (loop :for (k v) :on args :by #'cddr
                               :nconc `(,k '(lambda () ,v)))))
      `(list ',type
             ,(when options-p `(,@(first options/args)))
             ,@thunked-args))))

(defun process-node-data (data)
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
                  `(list ,@(mapcar #'process-trait-data traits)))
               ,(when children
                  `(list ,@(mapcar #'process-node-data children))))))))

(defun preprocess-data (name options data)
  (let ((data `(((,name ,@options) ,@data))))
    `(copy-tree (list ,@(mapcar #'process-node-data data)))))

(defun parse-trait-args (data)
  (flet ((%parse (data func)
           (apply #'u:dict
                  (mapcan
                   (lambda (x)
                     (destructuring-bind (type options . args) x
                       (declare (ignore options))
                       (list type
                             (u:plist->hash
                              (loop :for (k v) :on args :by #'cddr
                                    :collect k
                                    :collect (funcall func v))
                              :test #'eq))))
                   data))))
    (values (%parse data (lambda (x) (nth 2 x)))
            (%parse data (lambda (x) (compile nil x))))))

(defun parse-trait-options (data)
  (apply #'u:dict
         (mapcan
          (lambda (x)
            (destructuring-bind (type options . args) x
              (declare (ignore args))
              (list type options)))
          data)))

(defun parse-node-template (data path)
  (let* ((template-path (u:ensure-list (getf data :template)))
         (template-root-path (first template-path))
         (prefab (u:href =data= template-root-path)))
    (when template-path
      (unless (u:href =data= template-root-path)
        (error "Template ~s does not exist for prefab node ~{~a~^/~}." template-path path))
      (when (eq template-root-path (first path))
        (error "Cannot have a template to a node within the same prefab."))
      (or (u:href (pf.def::nodes prefab) template-path)
          (error "Template node ~{~a~^/~} not found while processing node: ~{~a~^/~}.~%~
                  This may mean that you just changed the tree structure of a prefab that another ~
                  templated node is trying to reference."
                 template-path
                 path)))))

(defun parse-nodes (prefab data &optional parent)
  (destructuring-bind (name options traits children) data
    (u:mvlet* ((nodes (pf.def::nodes prefab))
               (path `(,@parent ,name))
               (parent (u:href nodes (butlast path)))
               (args thunked-args (parse-trait-args traits))
               (trait-options (parse-trait-options traits))
               (template (parse-node-template options path))
               (node (pf.def::%make-node :prefab prefab
                                         :path path
                                         :parent parent
                                         :options (apply #'u:dict #'eq options)
                                         :template template
                                         :trait-options trait-options
                                         :trait-args args
                                         :trait-thunked-args thunked-args)))
      (setf (u:href nodes path) node)
      (map nil (lambda (x) (parse-nodes prefab x path)) children))))

(defun populate-explicit-nodes (prefab)
  (let ((name (pf.def::name prefab))
        (data (pf.def::data prefab))
        (nodes (pf.def::nodes prefab)))
    (map nil (lambda (x) (parse-nodes prefab x)) data)
    (setf (pf.def::root prefab) (u:href nodes (list name)))))

(defun populate-implicit-nodes (prefab)
  (let ((nodes (pf.def::nodes prefab)))
    (flet ((populate (parent template)
             (let ((template-path (pf.def::path template)))
               (u:do-hash (path node (pf.def::nodes (u:href =data= (car template-path))))
                 (let ((target-path (append (pf.def::path parent)
                                            (nthcdr (length template-path) path))))
                   (u:if-let ((target (u:href nodes target-path)))
                     (setf (pf.def::template target) node)
                     (copy-template node prefab target-path)))))))
      (u:do-hash-values (node (u:copy-hash-table nodes))
        (u:when-let ((template (pf.def::template node)))
          (populate node template))))))

(defun record-dependencies (prefab)
  (let ((old-masters (pf.def::masters prefab)))
    (setf (pf.def::masters prefab) nil)
    (u:do-hash-values (node (pf.def::nodes prefab))
      (u:when-let* ((template (pf.def::template node))
                    (template-prefab (pf.def::prefab template)))
        (pushnew (pf.def::name prefab) (pf.def::slaves template-prefab))
        (pushnew (pf.def::name template-prefab) (pf.def::masters prefab))))
    (dolist (master-spec old-masters)
      (let ((master (u:href =data= master-spec)))
        (unless (find master-spec (pf.def::masters prefab))
          (u:deletef (pf.def::slaves master) (pf.def::name prefab)))))))

(defun merge-traits (prefab)
  (u:do-hash-values (node (pf.def::nodes prefab))
    (u:when-let ((template (pf.def::template node)))
      (let ((args1 (pf.def::trait-args node))
            (args2 (pf.def::trait-args template))
            (thunks1 (pf.def::trait-thunked-args node))
            (thunks2 (pf.def::trait-thunked-args template))
            (policy (or (u:href (pf.def::options node) :policy) :overlay)))
        (merge-trait-args node args1 args2 policy)
        (merge-trait-args node thunks1 thunks2 policy)))))

(defun parse (prefab)
  (let (success-p)
    (unwind-protect
         (progn
           (populate-explicit-nodes prefab)
           (populate-implicit-nodes prefab)
           (record-dependencies prefab)
           (merge-traits prefab)
           (pf.fac::build prefab)
           (setf success-p t))
      (unless success-p
        (remhash (pf.def::name prefab) =data=)))))

(defun update (prefab)
  (parse prefab)
  ;; TODO
  ;; (enqueue :recompile (list :prefab (prefab-name prefab)))
  (dolist (spec (pf.def::slaves prefab))
    (u:when-let ((slave (u:href =data= spec)))
      (clrhash (pf.def::nodes slave))
      (update slave))))

(defun deregister-game-object (context game-object)
  (u:when-let ((prefab-name (gob::prefab-name game-object))
               (table (ctx::prefabs context)))
    (u:deletef (u:href table prefab-name) game-object)
    (unless (u:href table prefab-name)
      (remhash prefab-name table))))

(defun load-prefab (context name &key parent)
  (let ((prefabs =data=))
    (u:if-let ((prefab (u:href prefabs name)))
      (let* ((factory (pf.def::factory prefab))
             (game-object (funcall (pf.def::factory-func factory) context :parent parent)))
        (clrhash (pf.def::factory-game-objects factory))
        game-object)
      (error "Prefab ~s not defined." name))))

(defun find-reference-path (factory path)
  (let ((game-objects (pf.def::factory-game-objects factory))
        (current-path (pf.def::path (pf.def::factory-current-node factory))))
    (u:if-let ((index (search path current-path)))
      (u:href game-objects (subseq current-path 0 (+ index (length path))))
      (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}." path current-path))))

(defun find-reference-down (factory path &optional current)
  (let ((current-path (or current (pf.def::path (pf.def::factory-current-node factory)))))
    (or (u:href (pf.def::factory-game-objects factory) (append current-path path))
        (error "Failed to find reference ~{~a~^/~} for node ~{~a~^/~}." path current-path))))

(defun find-reference-up (factory level path)
  (let* ((current-path (pf.def::path (pf.def::factory-current-node factory)))
         (parent-path (butlast current-path level)))
    (if path
        (find-reference-down factory path parent-path)
        (or (u:href (pf.def::factory-game-objects factory) parent-path)
            (error "Failed to find parent reference ~{~a~^/~}." parent-path)))))

(defun generate-reference-func (reference-spec)
  (lambda (factory)
    (destructuring-bind (type . args) reference-spec
      (case type
        (:up
         (destructuring-bind (level &rest path) args
           (find-reference-up factory level path)))
        (:down
         (find-reference-down factory args))
        (:path
         (find-reference-path factory args))
        (t
         (error "Reference spec type must be :UP, :DOWN, or :PATH."))))))

(defmacro ref (&rest reference-spec)
  `(pf.def::%make-reference :func (generate-reference-func ',reference-spec)))

(defmacro define-prefab (name options &body body)
  (u:with-gensyms (data)
    (u:mvlet ((body decls doc (u:parse-body body :documentation t)))
      `(let ((,data ,(preprocess-data name options body)))
         (if (u:href =data= ',name)
             (reset ',name ,data)
             (make-prefab ',name ,data))
         (update (u:href =data= ',name))))))
