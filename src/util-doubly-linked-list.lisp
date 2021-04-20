(in-package #:%zed.utility.doubly-linked-list)

(defstruct (list
            (:constructor %make-list)
            (:conc-name nil)
            (:predicate nil)
            (:copier nil))
  (head nil :type (or node null))
  (tail nil :type (or node null))
  (%length 0 :type fixnum))

(defstruct (node
            (:conc-name nil)
            (:copier nil))
  value
  (previous nil :type (or node null))
  (next nil :type (or node null)))

(u:fn-> list-values (list) cl:list)
(defun list-values (list)
  (declare (optimize speed))
  (loop :for node = (head list) :then (next node)
        :while node
        :collect (value node)))

(u:define-printer (list stream :type nil)
  (format stream "~s" (list-values list)))

(u:define-printer (node stream)
  (format stream "~s" (value node)))

(u:fn-> length (list) fixnum)
(declaim (inline length))
(defun length (list)
  (declare (optimize speed))
  (%length list))

(u:fn-> find
        (list t &key (:start (or node null)) (:end (or node null)) (:key function)
              (:test function) (:from-end boolean))
        (or node null))
(defun find (list value &key start end (key #'identity) (test #'eql) from-end)
  (declare (optimize speed))
  (loop :for node = (or start (if from-end (tail list) (head list)))
          :then (if from-end (previous node) (next node))
        :while node
        :when (funcall test (funcall key (value node)) (funcall key value))
          :do (return node)
        :when (eq node end)
          :do (return)))

(u:fn-> insert (list t &key (:target (or node null)) (:where (member :before :after))) node)
(defun insert (list value &key target (where :after))
  (declare (optimize speed))
  (flet ((%insert (list previous next value)
           (let ((node (make-node :value value :previous previous :next next)))
             (if previous
                 (setf (next previous) node)
                 (setf (head list) node))
             (if next
                 (setf (previous next) node)
                 (setf (tail list) node))
             (incf (%length list))
             node)))
    (let ((target (or target (head list))))
      (ecase where
        (:before (%insert list (when target (previous target)) target value))
        (:after (%insert list target (when target (next target)) value))))))

(u:fn-> delete
        (list t &key (:key function) (:test function) (:from-end boolean))
        (values list boolean))
(defun delete (list object &key (key #'identity) (test #'eql) from-end)
  (declare (optimize speed))
  (flet ((%delete (list node)
           (let ((previous (when node (previous node)))
                 (next (when node (next node))))
             (if previous
                 (setf (next previous) next)
                 (setf (head list) next))
             (if next
                 (setf (previous next) previous)
                 (setf (tail list) previous))
             (decf (%length list))
             (values list t))))
    (if (node-p object)
        (%delete list object)
        (u:if-let ((node (find list object :key key :test test :from-end from-end)))
          (%delete list node)
          (values list nil)))))

(u:fn-> make-list (&rest t) list)
(defun make-list (&rest values)
  (declare (optimize speed))
  (loop :with list = (%make-list)
        :for value :in values
        :for node = (insert list value)
          :then (insert list value :target node)
        :finally (return list)))
