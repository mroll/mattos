;;; My Objects in Lisp
;;;


(defpackage :mattos
  (:use :cl)
  (:export :defobject
           :defmeth
           :this))

(in-package :mattos)


(defparameter *global-lookaside* (make-hash-table))

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

(defmacro with-hash-table (ht &body forms)
  (let ((symbs (gensym))
        (vals  (gensym))
        (s     (gensym)))
    `(let ((,symbs '())
           (,vals  '()))
       (maphash #'(lambda (k v)
                    (push (symb (format nil "~A" k)) ,symbs)
                    (push v ,vals))
                ,ht)
       (progv ,symbs ,vals
         ,@forms
         (dolist (,s ,symbs)
           (setf (gethash ,s ,ht) (symbol-value ,s)))))))

(defmacro instance-fn (varname classname)
  `(defun ,varname (prop &rest args)
     (let ((attrval (gethash prop this)))
       (cond ((null attrval) (do-method ,classname prop this args))
             ((null args) attrval)
             (t (setf (gethash prop this) (car (wrap-if-nil args))))))))

(defmacro class-mac (classname)
  `(defmacro ,classname (varname)
     `(let ((this (get-default-instance ',',classname)))
        (instance-fn ,varname ',',classname)
        (setf (gethash 'this this) #',varname))))

(defmacro defobject (classname slots &key (inherits nil))
  `(progn (defparameter ,(class-table-symb classname) (make-hash-table))
          (make-default-instance ',classname ',slots)
          (setf (gethash 'parent ,(class-table-symb classname)) ',inherits)
          (class-mac ,classname)))

(defmacro defmeth (classname name args &body body)
  (let* ((classtab (class-table-val classname))
         (oldmethod (gethash name classtab)))
    (invalidate-lookaside oldmethod)
    `(setf (gethash ',name ,(class-table-symb classname)) #'(lambda (,@args) ,@body))))

(defun hash-table-copy (ht)
  (let ((new-table (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash k new-table) v)) ht)
    new-table))

(defun get-default-instance (classname)
  (let* ((classtab (class-table-val classname))
         (parent   (gethash 'parent classtab))
         (default  (hash-table-copy (gethash 'default classtab))))
    (if (null parent)
      default
      (merge-hash-tables default
                         (get-default-instance parent)))))

;;; Key value pairs are copied from table2 into a copy of table1,
;;; unless table1 already contains that key.
(defun merge-hash-tables (table1 table2)
  (let ((new-table (hash-table-copy table1)))
    (maphash #'(lambda (k v) (if (null (gethash k new-table))
                               (setf (gethash k new-table) v)))
             table2)
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
  (maphash #'(lambda (k v) (if (equal methodval v)
                             (setf (gethash k *global-lookaside*) nil)))
           *global-lookaside*))

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
    (setf (symbol-function 'this) (gethash 'this this))
    (with-hash-table this
                     (if args
                       (apply m args)
                       (funcall m)))))

(defun find-meth (classname meth)
  (if (null classname)
    nil
    (let* ((classtab (class-table-val classname))
           (m (gethash meth classtab)))
      (if m
        m
        (find-meth (gethash 'parent classtab) meth)))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun wrap-if-nil (lst)
  (if (null lst)
    (list lst)
    lst))


; (defparameter h (make-hash-table))
; (setf (gethash 'a h) 1)
; (setf (gethash 'b h) 2)
; (setf (gethash 'c h) 3)
; (setf (gethash 'this h) #'(lambda (x) (+ x x)))
; (with-hash-table h
;                  (print (this 4))
;                  (setf a 5))

; 
; (dolist (s (list 'a 'b 'c))
;   (print (gethash s h)))


; (print (gethash h 'a))

; (defobject person ((name 'matt) age))
; 
; (defmeth person fly ()
;          (format t "People can't fly!~%"))
; 
; (defmeth person fly2 ()
;          (this 'fly))
; 
; (person matt)
; (matt 'fly2)

; 
; (defobject astronaut (helmet-size space-flights)
;            :inherits person)
; 
; (defmeth astronaut fly ()
;          (format t "Fly to the moon!~%"))
; 
; (astronaut matt)
; (matt 'fly)
