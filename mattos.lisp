;;; My Objects in Lisp
;;;


(defpackage :mattos
  (:use :cl)
  (:export :defobject
           :defmeth))

(in-package :mattos)


(defparameter *global-lookaside* (make-hash-table))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro with-hash-table (ht &body forms)
  (let ((symbs (gensym))
        (vals  (gensym)))
    `(let ((,symbs '())
           (,vals  '()))
       (maphash #'(lambda (k v)
                    (push (symb (format nil "~A" k)) ,symbs)
                    (push v ,vals))
                ,ht)
       (progv ,symbs ,vals
         ,@forms))))

(defmacro instance-fn (varname classname)
  `(defun ,varname (prop &rest args)
     (let ((attrval (gethash prop this)))
       (cond ((null attrval) (do-method ,classname prop this args))
             ((null args) attrval)
             (t (setf (gethash prop this) (car (wrap-if-nil args))))))))

(defmacro class-mac (classname)
  `(defmacro ,classname (varname)
     `(let ((this (get-default-instance ',',classname)))
        (instance-fn ,varname ',',classname))))

(defmacro defobject (classname slots &key (inherits nil))
  `(progn (defparameter ,(class-table-symb classname) (make-hash-table))
          (make-default-instance ',classname ',slots)
          (setf (gethash 'parent ,(class-table-symb classname)) ',inherits)
          (class-mac ,classname)))

(defmacro defmeth (classname name args body)
  (let* ((classtab (class-table-val classname))
         (oldmethod (gethash name classtab)))
    (invalidate-lookaside oldmethod)
    `(setf (gethash ',name ,(class-table-symb classname)) #'(lambda (,@args) ,body))))

(defun hash-table-copy (ht)
  (let ((new-table (make-hash-table)))
    (with-hash-table-iterator (get-entry ht)
                              (labels ((try (got-one &optional key val)
                                            (when got-one
                                              (setf (gethash key new-table) val))))
                                (multiple-value-call #'try (get-entry))))
    new-table))

(defun get-default-instance (classname)
  (let* ((classtab (class-table-val classname))
         (parent   (gethash 'parent classtab))
         (default  (hash-table-copy (gethash 'default classtab))))
    (if (null parent)
      default
      (merge-hash-tables default
                         (get-default-instance parent)))))

;;; Key value pairs are copied from table2 into table1, unless table1 already
;;; contains that key.
(defun merge-hash-tables (table1 table2)
  (let ((new-table (hash-table-copy table1)))
    (with-hash-table-iterator (get-entry table2)
                              (labels ((try (got-one &optional key val)
                                            (when got-one
                                              (if (null (gethash key new-table))
                                                (setf (gethash key new-table) val)))))
                                (multiple-value-call #'try (get-entry))))
    new-table))

(defun make-default-instance (classname slots)
  (let ((default (make-hash-table))
        (classtab (class-table-val classname)))
    (dolist (slot slots)
      (if (atom slot)
        (setf (gethash slot default) '(nil))
        (setf (gethash (car slot) default) (cadr slot))))
    (setf (gethash 'default classtab) default)))

(defun invalidate-lookaside (methodval)
  (with-hash-table-iterator (get-entry *global-lookaside*)
                            (labels ((invalidate (got-one &optional key val)
                                         (when got-one
                                           (if (equal methodval val)
                                             (setf (gethash key *global-lookaside*) nil)))))
                              (multiple-value-call #'invalidate (get-entry)))))

(defun class-table-symb (classname)
  (symb (format nil "*~a*" classname)))

(defun class-table-val (classname)
  (symbol-value (class-table-symb classname)))

(defun lookaside-key (classname methname)
  (symb (format nil "~a-~a" classname methname)))

(defun cache-method (classname methname methval)
  (setf (gethash (lookaside-key classname methname) *global-lookaside*)
        methval))

(defun get-cached-method (classname methname)
  (gethash (lookaside-key classname methname) *global-lookaside*))

(defun do-method (classname methname this args)
  (let* ((classtab (class-table-val classname))
         (m (gethash methname classtab)))
    (when (null m)
      (cache-method classname methname (find-meth (gethash 'parent classtab) methname))
      (setf m (get-cached-method classname methname)))
    (with-hash-table this
                     (if args
                       (apply m args)
                       (funcall m)))))

(defun find-meth (classname meth)
  (if (null classname)
    nil
    (let* ((classtab (class-table-val classname))
           (m (gethash meth classtab)))
      (if (null m)
        (find-meth (gethash 'parent classtab) meth)
        m))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun wrap-if-nil (lst)
  (if (null lst)
    (list lst)
    lst))

;;; (defobject person ((name 'matt) age))
;;; 
;;; (defmeth person fly ()
;;;          (format t "People can't fly!~%"))
;;; 
;;; (defobject astronaut (helmet-size space-flights)
;;;            :inherits person)
;;; 
;;; (defmeth astronaut fly ()
;;;          (format t "Fly to the moon!~%"))
;;; 
;;; (astronaut matt)
;;; (matt 'fly)
