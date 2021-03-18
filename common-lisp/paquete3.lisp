;1
(defun elem-in-pos (elemento lista posicion)
  (cond ((null lista) NIL)
	((= posicion 0)
	 (if (equal (first lista) elemento) T NIL))
	(T (elem-in-pos elemento (rest lista) (1- posicion)))))
(trace elem-in-pos)
(elem-in-pos '5 '(B A  D X -1 11) '3)
(elem-in-pos 'W '() '6)
(elem-in-pos '(A B) '(B A (7 8) -1 11) '4)
(elem-in-pos '(5 6) '(-1 3 2 (5 6) -7 12 33) '3)

;2
(defun inicio-en (lista elemento)
  (let ((respuesta '()))
    (cond ((null lista) 
	   (return-from respuesta))
	  ((equal (first lista) elemento)

;3

;4

(defun primer-impar-aux (lista contador)
  (cond ((null lista) (list NIL NIL))
	((and (numberp (first lista)) (= (mod (first lista) 2) 1))
	 (list (first lista) contador))
	(T (primer-impar-aux (rest lista) (1+ contador)))))

(defun primer-impar (lista)
  (primer-impar-aux lista 0))

(trace primer-impar-aux)
(trace primer-impar)

(primer-impar '(A B C D 10))
(primer-impar '(2 4 6 8 A  #\C 91 16 ))
(primer-impar '(2 4 3 8  #\W #\D #\X #\K ))
(primer-impar '())


