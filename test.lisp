(load "mattos.lisp")

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))



(mattos:defobject person ((name 'matt)
                          (age  21)))

(mattos:defmeth person greet (name)
                (format t "Hello, ~A!~%" name))

(mattos:defobject astronaut (helmet-size
                             (space-flights 0))
                  :inherits person)

(astronaut matt)
(format t "~A~%" (matt 'helmet-size))
(matt 'greet "larry")

(mattos:defmeth person greet (name)
                (format t "Bonjour, ~A!~%" name))

(matt 'greet "larry")

(mattos:defobject square
                  (side color))

(mattos:defmeth square area ()
                (* side side))

(square s)
(s 'side 12)
(format t "~A~%" (s 'area))

(mattos:defmeth square testprint ()
                  (format t "TESTPRINT~%"))

(mattos:defmeth square othertestprint ()
                (mattos:this 'testprint))

(s 'othertestprint)

; 
; (macrolet ((this (arg)
;                  `(funcall #'(lambda (x) (format t "~A~%" x)) ,arg)))
;   (progv (list 'x 'y 'this) (list 1 2 3)
;     (this this)))
