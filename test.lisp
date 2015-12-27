(load "mattos.lisp")


(mattos:defobject person
                  (name age))

(mattos:defmeth person greet (name)
                (format t "Hello, ~A!~%" name))

(mattos:defobject astronaut
                  (helmet-size space-flights)
                  :inherits person)

(astronaut matt)
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
